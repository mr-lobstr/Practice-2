#ifndef MODULAR_ARITHMETIC_H
#define MODULAR_ARITHMETIC_H

#include <cmath>
#include <tuple>
#include <stdexcept>


using num_type = long long;


num_type mod (num_type a, num_type b);

bool is_prime (num_type);

num_type pow_mod_Fermats (num_type a, num_type x, num_type p);

num_type pow_mod(num_type a, num_type x, num_type p);

std::tuple<num_type, num_type, num_type>
nod_extended (num_type a, num_type b);

num_type inverse_mod (num_type a, num_type m);

num_type last_digit (num_type a, num_type b, num_type c);

#endif