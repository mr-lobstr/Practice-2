#ifndef SHAMIR_SCHEME_H
#define SHAMIR_SCHEME_H

#include <vector>
#include <cmath>
#include <set>
#include "modular_arithmetic.h"

using key_type = std::pair<num_type, num_type>;

// генерирует n различных ключей,
// k  из которых необходимо для восстановления секрета
std::vector<key_type>
keys_generation (num_type secret, int n, int k, num_type m);


// восстанавливает секрет по переданным ключам
num_type secret_recovery (const std::vector<key_type>& points, num_type m);

// кодирует и декодирует строку, используя секретный ключ
void encode_and_decode (std::string& s, num_type secret_key, num_type M);


#endif