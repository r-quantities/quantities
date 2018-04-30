#ifndef QUANTITIES_PARSE
#define QUANTITIES_PARSE

#include <cmath>

enum NumberState {
  STATE_VAL_PRE,
  STATE_VAL_LHS,
  STATE_VAL_RHS,
  STATE_VAL_FIN,
  STATE_ERR_PRE,
  STATE_ERR_LHS,
  STATE_ERR_RHS,
  STATE_ERR_FIN,
  STATE_EXP
};

inline void add_digit(double& x, const char& c) {
  x *= 10;
  x += c - '0';
}

inline void add_digit(double& x, double& d, const char& c) {
  d *= 10;
  x += (c - '0') / d;
}

template <typename Iterator>
inline void skip_space(Iterator& pos) {
  while (*pos == ' ') pos++;
}

template <typename Iterator>
bool parseErrors(Iterator& first, Iterator& last, double& val, double& err) {
  NumberState state = STATE_VAL_PRE;
  Iterator cur = first;
  double val_denom = 1, err_denom = 1, exponent = 0;
  bool seenNumber = false, exp_init = true, pm = false;
  double sign = 1.0, exp_sign = 1.0;
  int decimals = 0;

  char decimalMark = '.', groupingMark = ',';

  for (; cur != last; ++cur) {
    switch (state) {
    case STATE_VAL_PRE:
      skip_space(cur);
      if (*cur == '(') {
        // do nothing
      } else if (*cur == '-') {
        state = STATE_VAL_LHS;
        sign = -1.0;
      } else if (*cur == decimalMark) {
        state = STATE_VAL_RHS;
      } else if (*cur >= '0' && *cur <= '9') {
        seenNumber = true;
        state = STATE_VAL_LHS;
        add_digit(val, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_VAL_LHS:
      if (*cur == groupingMark) {
        // do nothing
      } else if (*cur == decimalMark) {
        state = STATE_VAL_RHS;
      } else if (*cur == ' ') {
        state = STATE_VAL_FIN;
      } else if (seenNumber && *cur == '(') {
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == "±"[0] && *(++cur) == "±"[1]) {
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == '+') {
        if (*(++cur) == '/') cur++;
        if (*cur != '-') goto end;
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && (*cur == 'e' || *cur == 'E')) {
        state = STATE_EXP;
      } else if (*cur >= '0' && *cur <= '9') {
        seenNumber = true;
        add_digit(val, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_VAL_RHS:
      if (*cur == groupingMark) {
        // do nothing
      } else if (*cur == ' ') {
        state = STATE_VAL_FIN;
      } else if (seenNumber && *cur == '(') {
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == "±"[0] && *(++cur) == "±"[1]) {
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == '+') {
        if (*(++cur) == '/') cur++;
        if (*cur != '-') goto end;
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && (*cur == 'e' || *cur == 'E')) {
        state = STATE_EXP;
      } else if (*cur >= '0' && *cur <= '9') {
        seenNumber = true;
        decimals++;
        add_digit(val, val_denom, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_VAL_FIN:
      skip_space(cur);
      if (seenNumber && *cur == '(') {
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == "±"[0] && *(++cur) == "±"[1]) {
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && *cur == '+') {
        if (*(++cur) == '/') cur++;
        if (*cur != '-') goto end;
        pm = true;
        state = STATE_ERR_PRE;
      } else if (seenNumber && (*cur == 'e' || *cur == 'E')) {
        state = STATE_EXP;
      } else {
        goto end;
      }
      break;
    case STATE_ERR_PRE:
      skip_space(cur);
      if (*cur == decimalMark) {
        state = STATE_ERR_RHS;
      } else if (*cur >= '0' && *cur <= '9') {
        state = STATE_ERR_LHS;
        add_digit(err, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_ERR_LHS:
      if (*cur == groupingMark) {
        // do nothing
      } else if (*cur == decimalMark) {
        state = STATE_ERR_RHS;
      } else if (*cur == ' ' || *cur == ')') {
        state = STATE_ERR_FIN;
      } else if (*cur >= '0' && *cur <= '9') {
        add_digit(err, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_ERR_RHS:
      if (*cur == groupingMark) {
        // do nothing
      } else if (*cur == ' ' || *cur == ')') {
        state = STATE_ERR_FIN;
      } else if (*cur >= '0' && *cur <= '9') {
        add_digit(err, err_denom, *cur);
      } else {
        goto end;
      }
      break;
    case STATE_ERR_FIN:
      skip_space(cur);
      if (*cur == ')') {
        // do nothing
      } else if (*cur == 'e' || *cur == 'E') {
        state = STATE_EXP;
      } else {
        goto end;
      }
      break;
    case STATE_EXP:
      // negative/positive sign only allowed immediately after 'e' or 'E'
      if (*cur == '-' && exp_init) {
        exp_sign = -1.0;
        exp_init = false;
      } else if (*cur == '+' && exp_init) {
        // sign defaults to positive
        exp_init = false;
      } else if (*cur >= '0' && *cur <= '9') {
        add_digit(exponent, *cur);
        exp_init = false;
      } else {
        goto end;
      }
      break;
    }
  }

end:
  skip_space(cur);
  first = cur;
  val *= sign;

  if (!pm && decimals && std::floor(err) == err)
    err *= std::pow(10.0, -decimals);

  if (exponent) {
    val *= std::pow(10.0, exp_sign * exponent);
    err *= std::pow(10.0, exp_sign * exponent);
  }

  return seenNumber;
}

#endif
