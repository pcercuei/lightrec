cmake_minimum_required(VERSION 3.0)
project(lightrec LANGUAGES C VERSION 0.1)

set(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libraries")

if (NOT LOG_LEVEL)
	set(LOG_LEVEL Info CACHE STRING "Log level" FORCE)
	set_property(CACHE LOG_LEVEL PROPERTY STRINGS NoLog Error Warning Info Debug)
endif()

if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
		"Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel."
		FORCE)
	set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS None Debug Release RelWithDebInfo MinSizeRel)
endif()

string(TOUPPER ${LOG_LEVEL} LIGHTREC_LOG_LEVEL)
add_definitions(-DLOG_LEVEL=${LIGHTREC_LOG_LEVEL}_L)

list(APPEND LIGHTREC_SOURCES
	blockcache.c
	disassembler.c
	emitter.c
	interpreter.c
	lightrec.c
	optimizer.c
	regcache.c
)
list(APPEND LIGHTREC_HEADERS
	blockcache.h
	debug.h
	disassembler.h
	emitter.h
	interpreter.h
	lightrec-private.h
	lightrec.h
	optimizer.h
	recompiler.h
	regcache.h
)

option(ENABLE_FIRST_PASS "Run the interpreter as first-pass optimization" ON)

option(ENABLE_THREADED_COMPILER "Enable threaded compiler" ON)
if (ENABLE_THREADED_COMPILER)
	list(APPEND LIGHTREC_SOURCES recompiler.c)

	if (NOT ENABLE_FIRST_PASS)
		message(SEND_ERROR "Threaded compiler requires first-pass optimization")
	endif (NOT ENABLE_FIRST_PASS)
endif (ENABLE_THREADED_COMPILER)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
configure_file(config.h.cmakein config.h @ONLY)

add_library(${PROJECT_NAME} ${LIGHTREC_SOURCES} ${LIGHTREC_HEADERS})
set_target_properties(${PROJECT_NAME} PROPERTIES
	VERSION ${PROJECT_VERSION}
	SOVERSION ${PROJECT_VERSION_MAJOR}
	PUBLIC_HEADER lightrec.h
	C_STANDARD 11
	C_STANDARD_REQUIRED ON
	C_EXTENSIONS OFF
)

find_library(LIBLIGHTNING lightning REQUIRED)
find_path(LIBLIGHTNING_INCLUDE_DIR lightning.h REQUIRED)

include_directories(${LIBLIGHTNING_INCLUDE_DIR})
target_link_libraries(${PROJECT_NAME} PRIVATE ${LIBLIGHTNING})

if (LOG_LEVEL STREQUAL Debug)
	find_library(LIBOPCODES NAMES opcodes-multiarch opcodes)
	find_path(LIBOPCODES_INCLUDE_DIR dis-asm.h)

	if (NOT LIBOPCODES OR NOT LIBOPCODES_INCLUDE_DIR)
		message(SEND_ERROR "Debug log level requires libopcodes (from binutils) to be installed.")
	endif ()

	set(ENABLE_DISASSEMBLER ON)
	include_directories(${LIBOPCODES_INCLUDE_DIR})
	target_link_libraries(${PROJECT_NAME} PRIVATE ${LIBOPCODES})
endif()

include(GNUInstallDirs)
install(TARGETS ${PROJECT_NAME}
	ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
	RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
