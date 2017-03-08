/*
	A class used to swap string and double.
	Liangjun, Zhu
	zlj@lreis.ac.cn
	Lreis, CAS  
	Feb 24, 2016
	reference: http://www.adp-gmbh.ch/cpp/chameleon.html
*/
#pragma once
#ifndef SWAP_STRING_DOUBLE
#define SWAP_STRING_DOUBLE
#include <string>
using namespace std;
class SwapStringDouble{
public:
	SwapStringDouble(){};
	explicit SwapStringDouble(const string&);
	explicit SwapStringDouble(double);
	explicit SwapStringDouble(const char*);

	SwapStringDouble(const SwapStringDouble&);
	SwapStringDouble& operator = (SwapStringDouble const&);

	SwapStringDouble& operator = (double);
	SwapStringDouble& operator = (string const&);
public:
	operator string() const;
	operator double() const;
private:
	string value_;
};
#endif