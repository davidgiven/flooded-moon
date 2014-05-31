#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calculon.h"

typedef Calculon::Instance<Calculon::RealIsDouble> Compiler;
typedef Compiler::Real Real;

static Compiler::StandardSymbolTable symbols;
static std::string lastError;

struct Function
{
	typedef void AnonymousFunction(void);
	Compiler::Program<AnonymousFunction> compiledFunction;

	Function(const char* code, const char* signature):
		compiledFunction(symbols, code, signature)
	{}


};

extern "C" const char* ada_calculon_last_error(void)
{
	return lastError.c_str();
}

extern "C" Function* ada_calculon_create(const char* code,
		const char* signature)
{
	try
	{
		Function* f = new Function(code, signature);
		//f->compiledFunction.dump();
		return f;
	}
	catch (const Compiler::CompilationException e)
	{
		lastError = e.what();
		return NULL;
	}
}

extern "C" void ada_calculon_destroy(Function* fn)
{
	// Something weird is going wrong where LLVM seg faults on
	// destruction. So just leak functions for now.
	//delete fn;
}

typedef void (*Func)();

extern "C" Func ada_calculon_get_pointer(Function* fn)
{
	return fn->compiledFunction;
}

