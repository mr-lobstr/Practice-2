#include <iostream> 
#include "modular_arithmetic.h"
using namespace std;

int main()
{
	num_type a, x, p, b;
    
    cout << "Сравнение числа a в степени x "
         << "по модулю простого числа p." << endl
         << "Введите a, x и p: " << endl;
    cin >> a >> x >> p;
    
    cout << "Алгоритм, основанный на теореме Ферма: ";

    try
    {   
        cout << pow_mod_Fermats(a, x, p) << endl;
    }
    catch (const invalid_argument& exc)
    {
        cout << exc.what() << endl;
    }

    cout << "Через разложение степени: " << pow_mod(a, x, p) << endl;

    cout << "\nРасширенный алгоритм Евклида." << endl
         << "Введите a и b: ";
    cin >> a >> b;

    auto [v, u, d] = nod_extended(a, b);
    cout << "НОД(a, b) = " << d << endl
         << "v*a + u*b = d: "
         << v << "*" << a << " + " << u << "*" << b
         << " = " << d << endl;

    
    cout << "\nЧисло обратное c по модулю m" << endl
         << "Введите c и m: ";
    cin >> a >> p;

    try
    {
        cout << "Обратное " << a << " по модулю " << p
         << " = " << inverse_mod(a, p) << endl;
    }
    catch (const invalid_argument& exc)
    {
        cout << exc.what() << endl;
    }

    
    cout << "\nПоследняя цифра трёхэтажного числа: ";
    cin >> a >> b >> p;
    cout << last_digit(a, b, p) << endl;
}