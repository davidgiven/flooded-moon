POVRAY = ../povray/.obj/unix/povray
PLACE = jura
PARAMS = \

all: newmoon.png

newmoon.png: Makefile newmoon.ini newmoon.inc newmoon.pov earth.inc \
		/tmp/camera.inc terrainlib/moon.so $(POVRAY)
	$(POVRAY) +Inewmoon +A +P +W400 +H300
	
/tmp/camera.inc: Makefile terrainmaker/terrainmaker \
		$(wildcard scripts/*) $(wildcard places/*)
	./terrainmaker/terrainmaker scripts/data.tm scripts/makecamera.tm places/$(PLACE).tm $(PARAMS)
	
terrainmaker/terrainmaker: $(wildcard terrainmaker/*.h) $(wildcard terrainmaker/*.cc)
	make -C terrainmaker terrainmaker

terrainlib/moon.so: terrainlib/moon.cc $(wildcard terrainlib/include/*.h)
	gcc -O3 -shared -fpic -fPIC -o terrainlib/moon.so terrainlib/moon.cc -lnoise

