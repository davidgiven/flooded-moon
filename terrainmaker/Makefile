CXX = clang++
LLVM = -I$(shell llvm-config-3.2 --includedir) -lLLVM-3.2
CALCULON = -I/home/dg/nonshared/calculon/include

LIBS = \
	$(CALCULON) \
	$(LLVM) \
	-lpng12 \
	-lnoise \
	-lboost_program_options \
	-lboost_iostreams

all: terrainmaker terrainmaker-dbg

terrainmaker: terrainmaker.cc Makefile $(wildcard *.h)
	$(CXX) -std=c++11 -O3 -DNDEBUG -fomit-frame-pointer -o $@ $< $(LIBS)
	
terrainmaker-dbg: terrainmaker.cc Makefile $(wildcard *.h)
	$(CXX) -std=c++11 -g -o $@ $< $(LIBS)
	
	
