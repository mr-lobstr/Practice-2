#include <iostream>
#include "modular_arithmetic.h"
using namespace std;

num_type mod (num_type a, num_type b)
{
    return a >= 0 ? a % b 
                  : b + (a % b);
}


bool is_prime (num_type n)
{
    if (n == 2)
        return true;

    if (n < 2 or mod(n, 2) == 0)
        return false;

    for (num_type i = 3; i <= sqrt(n); ++i)
    {
        if (mod(n, i) == 0)
            return false;
    }

    return true;
}


num_type pow_mod_Fermats (num_type a, num_type x, num_type p)
{
    if (mod(a, p) == 0)
        throw invalid_argument("ошибка: a делиться на p");

    if (not is_prime(p))
        throw invalid_argument("ошибка: p - составное число");

    num_type result = 1;
    x = mod(x, p - 1);

    for (num_type i = 0; i < x; ++i)
    {
        result = mod(result * a, p);
    }

    return result;
}


num_type pow_mod(num_type a, num_type x, num_type p)
{
    num_type result = 1;

    while (x > 0)
    {
        if (mod(x, 2) == 1)
            result = mod(result * a, p);

        a = mod(a * a, p);
        x /= 2;
    }

    return result;
}

num_type nod (num_type a, num_type b)
{
    while (a % b != 0)
    {
        num_type oldB = b;
        b = a % b;
        a = oldB;
    }

    return b;
}

tuple<num_type, num_type, num_type>
nod_extended (num_type a, num_type b, bool print)
{
    num_type vPrev = 1, uPrev = 0;
    num_type v = 0, u = 1;
    num_type r;

    while ((r = mod(a, b)) != 0)
    {
        num_type tV = vPrev;
        num_type tU = uPrev;

        vPrev = v;
        uPrev = u;

        v = tV - (a / b) * vPrev;
        u = tU - (a / b) * uPrev;

        if (print)
            cout << a << " " << v << " " << b << " " << u
                 << " " << r << endl;

        a = b, b = r;
    }

    if (print)
        cout << a << " " << v << " " << b << " " << u
                  << " " << r << endl; 

    return make_tuple(v, u, b);
}

num_type inverse_mod (num_type a, num_type m)
{
    auto [v, u, d] = nod_extended(a, m, false);

    if (d != 1)
        throw invalid_argument("ошибка, числа a и m должны быть взаимно просты");

    return mod(v, m);
}

num_type func_Euler (num_type m)
{
    num_type cnt = 0;

    for (num_type i = 2; i < m; ++i)
    {
        if (nod(m, i) == 1)
            ++cnt;
    }
    
    return cnt;
}

num_type last_digit (num_type a, num_type b, num_type c)
{
    num_type pwr = pow_mod(b, c, func_Euler(10));
    return pow_mod(a, pwr, 10);
}