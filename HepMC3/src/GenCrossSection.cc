// -*- C++ -*-
//
// This file is part of HepMC
// Copyright (C) 2014-2015 The HepMC collaboration (see AUTHORS for details)
//
/**
 *  @file GenCrossSection.cc
 *  @brief Implementation of \b class GenCrossSection
 *
 */
#include "HepMC/GenCrossSection.h"
#include <cstring> // memcmp
#include <cstdlib> // atoi
#include <cstdio> // sprintf

namespace HepMC {

bool GenCrossSection::from_string(const string &att) {
    const char *cursor = att.data();

    cross_section = atof(cursor);

    if( !(cursor = strchr(cursor+1,' ')) ) return false;
    cross_section_error = atof(cursor);

    return true;
}

bool GenCrossSection::to_string(string &att) const {
    char buf[64];

    sprintf(buf,"%.8e %.8e",
            cross_section,
            cross_section_error);

    att = buf;

    return true;
}

bool GenCrossSection::operator==( const GenCrossSection& a ) const {
  return ( memcmp( (void*)this, (void*) &a, sizeof(class GenCrossSection) ) == 0 );
}

bool GenCrossSection::operator!=( const GenCrossSection& a ) const {
    return !( a == *this );
}

bool GenCrossSection::is_valid() const {
    if( cross_section       != 0 ) return true;
    if( cross_section_error != 0 ) return true;
    return false;
}

} // namespace HepMC
