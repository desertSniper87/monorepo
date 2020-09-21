
LIBKX9R_CODE = libcx9r/aes256.c libcx9r/base64.c libcx9r/kdbx.c libcx9r/key_tree.c libcx9r/salsa20.c libcx9r/sha256.c libcx9r/stream.c libcx9r/util.c
DEFINES = -DHAVE_STDINT_H -DGCRYPT_WITH_SHA256 -DGCRYPT_WITH_AES -DBYTEORDER=1234 -DHAVE_EXPAT

kdbxviewer: $(LIBKX9R_CODE) src/main.c src/tui.c src/mainWindow.stfl src/helper.c
	mkdir -p bin
	gcc -g -o bin/kdbxviewer -I./include/ -I./libcx9r/ src/main.c src/helper.c $(DEFINES) $(LIBKX9R_CODE) src/tui.c -lgcrypt -lexpat -lz -lstfl -lncursesw -lmenu -Wno-pointer-sign

install:
	mkdir -p $(DESTDIR)/usr/bin
	cp bin/kdbxviewer $(DESTDIR)/usr/bin