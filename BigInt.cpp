// Signed multiple precision big integer multiplication
#include <cstdint>
#include <vector>
#include <algorithm>
#include <iostream>
#include <string>

// #include <cstdlib>
// #include <fstream>
// #include <unistd.h>
// #include <gmp.h>

// std::string calculate(const std::string &a, const std::string &b, char op) {
//     mpz_t ma, mb;
//     mpz_init_set_str(ma, a.c_str(), 10);
//     mpz_init_set_str(mb, b.c_str(), 10);
//     if (op == '+') {
//         mpz_add(ma, ma, mb);
//     } else if (op == '-') {
//         mpz_sub(ma, ma, mb);
//     } else if (op == '*') {
//         mpz_mul(ma, ma, mb);
//     }

//     std::string res = mpz_get_str(NULL, 10, ma);

//     return res;

//     // char tempFile[] = "/tmp/bigint-XXXXXX";
//     // mkstemp(tempFile);
//     // std::string cmd = "BC_LINE_LENGTH=0 bc <<< '" + a + " " + op + " " + b + "' > " + tempFile;
//     // system(cmd.c_str());
//     // std::ifstream fs(tempFile);
//     // std::string res;
//     // fs >> res;
//     // unlink(tempFile);
//     // return res;
// }

class BigInt {
#ifdef __SIZEOF_INT128__
    typedef unsigned __int128 value_type;
    typedef uint64_t truncate_type;
    const static value_type BASE = (value_type)10000000000000000000ull;
    const static size_t BASE_LENGTH = 19;
    // const static value_type BASE = (value_type)100;
    // const static size_t BASE_LENGTH = 2;
#else
    typedef uint64_t value_type;
    typedef uint32_t truncate_type;
    const static value_type BASE = 1000000000ull;
    const static size_t BASE_LENGTH = 9;
#endif
    std::vector<value_type> digits;
    bool isMinus;

    BigInt() : isMinus(false) {}

    BigInt(std::vector<value_type> &&digits, bool isMinus = false) : digits(digits), isMinus(isMinus) {}

    void removeLeadingZeros() {
        while (digits.back() == 0) digits.pop_back();
    }

public:
    BigInt(int64_t x) {
        if (x >= 0) {
            isMinus = false;
            digits.push_back(x);
        } else {
            isMinus = true;
            digits.push_back(-x);
        }
    }

    BigInt(std::string str) {
        if (str.length() == 0) {
            isMinus = false;
            digits.push_back(0);
            return;
        }

        if (str[0] == '-') str = str.substr(1), isMinus = true;
        else isMinus = false;

        size_t digitCount = (str.length() / BASE_LENGTH) + !!(str.length() % BASE_LENGTH);
        for (size_t i = 0, l = str.length() - BASE_LENGTH; i < digitCount; i++, l -= BASE_LENGTH) {
            value_type x = 0;
            for (size_t j = l > str.length() ? 0 : l; j < l + BASE_LENGTH; j++)
                x = x * 10 + str[j] - '0';
            digits.push_back(x);
        }
    }

    bool isZero() const {
        return digits.size() == 0 || (digits.size() == 1 && digits[0] == 0);
    }

    operator std::string() const {
        if (isZero()) return "0";

        std::string str;
        for (size_t i = digits.size() - 1; i < digits.size(); i--) {
            char tmp[BASE_LENGTH];
            value_type x = digits[i];
            for (size_t j = BASE_LENGTH - 1; j < BASE_LENGTH; j--) tmp[j] = x % 10 + '0', x /= 10;

            if (i == digits.size() - 1) {
                // Remove leading zeros
                size_t zeros = 0;
                while (tmp[zeros] == '0' && zeros < BASE_LENGTH) zeros++;
                str.append(tmp + zeros, BASE_LENGTH - zeros);
            } else {
                str.append(tmp, BASE_LENGTH);
            }
        }

        return isMinus ? ("-" + str) : str;
    }

    friend BigInt unsignedAdd(const BigInt &a, const BigInt &b, bool resultIsMinus) {
        BigInt res;
        res.digits.resize(std::max(a.digits.size(), b.digits.size()) + 1);
        for (size_t i = 0; i < res.digits.size(); i++) {
            if (i < a.digits.size()) res.digits[i] += a.digits[i];
            if (i < b.digits.size()) res.digits[i] += b.digits[i];
            if (res.digits[i] >= BASE) {
                res.digits[i + 1]++;
                res.digits[i] -= BASE;
            }
        }
        res.removeLeadingZeros();

        res.isMinus = resultIsMinus;
        return res;
    }

    friend BigInt unsignedSub(const BigInt &a, const BigInt &b, bool resultIsMinus) {
        BigInt res;
        res.digits.resize(a.digits.size());
        for (size_t i = 0; i < res.digits.size(); i++) {
            res.digits[i] += a.digits[i];
            if (i < b.digits.size()) res.digits[i] -= b.digits[i];

            // Overflow?
            if (res.digits[i] > a.digits[i]) {
                res.digits[i + 1]--;
                res.digits[i] += BASE;
            }
        }
        res.removeLeadingZeros();

        res.isMinus = resultIsMinus;
        return res;
    }

    friend BigInt unsignedMul(const BigInt &a, const BigInt &b, bool resultIsMinus) {
        const BigInt &shorter = a.digits.size() < b.digits.size() ? a : b,
                     &longer = a.digits.size() < b.digits.size() ? b : a;
        if (shorter.isZero()) {
            return BigInt(0);
        } else if (shorter.digits.size() == 1) {
            BigInt res;
            res.digits.resize(longer.digits.size() + 1);
            truncate_type single_value = (truncate_type)shorter.digits[0];
            for (size_t i = 0; i < longer.digits.size(); i++) {
                res.digits[i] += (value_type)single_value * (truncate_type)longer.digits[i];
                res.digits[i + 1] += res.digits[i] / BASE;
                res.digits[i] %= BASE;
            }
            res.removeLeadingZeros();

            res.isMinus = resultIsMinus;
            return res;
        } else {
            // longer = l1 * BASE ^ n + l2, shorter = s1 * BASE ^ n + s2
            size_t n = longer.digits.size() / 2;
            BigInt l1 = std::vector<value_type>(longer.digits.begin() + n, longer.digits.end()),
                   l2 = std::vector<value_type>(longer.digits.begin(), longer.digits.begin() + n),
                   s1 = shorter.digits.size() > n ? std::vector<value_type>(shorter.digits.begin() + n, shorter.digits.end()) : std::vector<value_type>(),
                   s2 = shorter.digits.size() > n ? std::vector<value_type>(shorter.digits.begin(), shorter.digits.begin() + n) : std::vector<value_type>(shorter.digits);
            
            l2.removeLeadingZeros();
            s2.removeLeadingZeros();

            // result = l1 * s1 * BASE ^ 2n + ((l1 - l2) * (s2 - s1) + l1 * s1 + l2 * s2) * BASE ^ n + l2 * s2;
            //        = left + middleSum * BASE * n + right
            //        = left + middle + right
            BigInt l1s1 = unsignedMul(l1, s1, false),
                   l2s2 = unsignedMul(l2, s2, false),
                   l1l2s2s1 = (l1 - l2) * (s2 - s1);
            
            BigInt left;
            if (!l1s1.isZero()) {
                left.digits.resize(n * 2);
                left.digits.insert(left.digits.end(), l1s1.digits.begin(), l1s1.digits.end());
            }

            BigInt middleSum = l1l2s2s1 + l1s1 + l2s2;
            BigInt middle;
            if (!middleSum.isZero()) {
                middle.digits.resize(n);
                middle.digits.insert(middle.digits.end(), middleSum.digits.begin(), middleSum.digits.end());
            }

            BigInt &right = l2s2;

            BigInt res = left + middle + right;

            res.removeLeadingZeros();

            res.isMinus = resultIsMinus;
            return res;
        }
    }

    friend int unsignedCompare(const BigInt &a, const BigInt &b) {
        if (a.digits.size() == b.digits.size()) {
            for (size_t i = a.digits.size() - 1; i < a.digits.size(); i--) if (a.digits[i] != b.digits[i]) return a.digits[i] < b.digits[i] ? -1 : 1;
            return 0;
        } else return a.digits.size() < b.digits.size() ? -1 : 1;
    }

    friend BigInt operator+(const BigInt &a, const BigInt &b) {
        if (a.isMinus == b.isMinus) return unsignedAdd(a, b, a.isMinus);
        else switch (unsignedCompare(a, b)) {
        case -1:
            return unsignedSub(b, a, b.isMinus);
        case 1:
            return unsignedSub(a, b, a.isMinus);
        default:
            return BigInt(0);
        }
    }

    friend BigInt operator-(const BigInt &a, const BigInt &b) {
        if (a.isMinus == !b.isMinus) return unsignedAdd(a, b, a.isMinus);
        else switch (unsignedCompare(a, b)) {
        case -1:
            return unsignedSub(b, a, !b.isMinus);
        case 1:
            return unsignedSub(a, b, a.isMinus);
        default:
            return BigInt(0);
        }
    }

    friend BigInt operator*(const BigInt &a, const BigInt &b) {
        return unsignedMul(a, b, a.isMinus != b.isMinus);
    }
};

int main() {
    std::string str1, str2;
    std::cin >> str1 >> str2;

    BigInt x1 = str1, x2 = str2, res = x1 * x2;
    std::cout << (std::string)res << std::endl;

    // std::cout << calculate(str1, str2, '*') << std::endl;
}
