/*
	Implementation of swapStringDouble class.
*/
#include <string>
#include <sstream>
#include <iostream>
#include "SwapStringDouble.h"
SwapStringDouble::SwapStringDouble(string const& value){
	value_ = value;
}
SwapStringDouble::SwapStringDouble(const char* c){
	value_ = c;
}
SwapStringDouble::SwapStringDouble(double d){
	stringstream s;
	s<<d;
	value_ = s.str();
}
SwapStringDouble::SwapStringDouble(SwapStringDouble const& other){
	value_ = other.value_;
}
SwapStringDouble& SwapStringDouble::operator=(SwapStringDouble const& other){
	value_ = other.value_;
	return *this;
}
SwapStringDouble& SwapStringDouble::operator=(double i){
	stringstream s;
	s << i;
	value_ = s.str();
	return *this;
}
SwapStringDouble& SwapStringDouble::operator=(string const& s){
	value_ = s;
	return *this;
}
SwapStringDouble::operator string() const{
	return value_;
}
SwapStringDouble::operator double() const{
	return atof(value_.c_str());
}