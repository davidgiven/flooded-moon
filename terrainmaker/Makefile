
#CXX = g++
CXX = clang++

CXXFLAGS = \
	-std=c++11 \
	-I$(HOME)/nonshared/calculon/include \
	-I/usr/include/llvm-5.0 \
	-I/usr/include/llvm-c-5.0 \
	-I/usr/include/pcl-1.8 \
	-I/usr/include/eigen3 \

DEFS += \

LIBS += \
	$(shell pkg-config libpng --libs) \
	-L/usr/lib/llvm-5.0 \
	-lLLVM-5.0 \
	-lnoise \
	-lboost_program_options \
	-lboost_iostreams \
	-lboost_system \
	-lpng \
	-lpcl_io \
	-lpcl_common \

LIBSRCS = \
	sphericalroam.cc \
	writer.cc \
	plywriter.cc \
	povwriter.cc \
	map.cc \
	pds.cc \
	pdslabel.cc \
	pdsset.cc \
	matrix.cc \
	variables.cc \
	functions.cc \
	utils.cc

RELEASEOBJS = $(patsubst %.cc,.obj/release/%.o,$(LIBSRCS))
DEBUGOBJS = $(patsubst %.cc,.obj/debug/%.o,$(LIBSRCS))

DEPS = $(patsubst %.o,%.d,$(RELEASEOBJS) $(DEBUGOBJS))
	
all: terrainmaker terrainmaker-dbg

terrainmaker-dbg: .obj/debug/terrainmaker.o $(DEBUGOBJS)
	$(CXX) -g -o $@ $^ $(LIBS)
	
terrainmaker: .obj/release/terrainmaker.o $(RELEASEOBJS)
	$(CXX) -O3 -DNDEBUG -fomit-frame-pointer -o $@ $^ $(LIBS)
	
decloud: .obj/release/decloud.o $(RELEASEOBJS)
	$(CXX) -O3 -DNDEBUG -fomit-frame-pointer -o $@ $^ $(LIBS)

.obj/decloud/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -O3 -DNDEBUG -fomit-frame-pointer -o $@ $<

.obj/release/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o,%.d,$@) -c -O3 -DNDEBUG -fomit-frame-pointer -o $@ $<

.obj/debug/%.o: %.cc
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS)  -MMD -MF $(patsubst %.o,%.d,$@) -c -g -o $@ $<

clean:
	rm -rf .obj

-include $(DEPS)

