set(TARGET_NAME tonetest)
add_executable(${TARGET_NAME} main.cpp)
target_link_libraries(${TARGET_NAME} fssimplewindow ym2612 yssimplesound)

if(MSVC)
	target_link_libraries(${TARGET_NAME} yssimplesound_dsound)
elseif(APPLE)
	find_library(COCOA_LIB Cocoa)
	mark_as_advanced(COCOA_LIB)
	target_link_libraries(${TARGET_NAME} yssimplesound_macosx ${COCOA_LIB})
elseif(UNIX)
	target_link_libraries(${TARGET_NAME} yssimplesound_linux_alsa)
endif()
