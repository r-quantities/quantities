#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

template <typename T>
T exponent(T& x) {
  if (x < 0) return x = 0;
  if (x == 0) return 1;
  return std::floor(std::log10(x)) + 1;
}

template <typename Iterator>
bool parseErrors(Iterator& first, Iterator& last, double& val, double& err) {
  using boost::spirit::qi::double_;
  using boost::spirit::qi::long_;
  using boost::spirit::qi::ulong_;
  using boost::spirit::qi::char_;
  using boost::spirit::qi::eps;
  using boost::spirit::qi::lit;
  using boost::spirit::qi::_1;
  using boost::spirit::qi::rule;
  using boost::spirit::ascii::space_type;
  using boost::phoenix::ref;

  long LHS = 0, RHS = -1;
  int zeroes = 0;
  double exp = 0;
  bool pm = false;

  rule<Iterator, space_type> LHS_ = long_[ref(LHS)=_1];
  rule<Iterator, space_type> RHS_ = char_(".,") >>
    ( '0' % eps[ref(zeroes)++] || -ulong_[ref(RHS)=_1] );

  rule<Iterator, space_type> pm_ = lit("+/-") | lit("+-") | lit("±");
  rule<Iterator, space_type> err_ =
    ( '(' >> -pm_[ref(pm)=true] >> double_[ref(err)=_1] >> ')' ) |
    ( pm_[ref(pm)=true] >> double_[ref(err)=_1] );

  rule<Iterator, space_type> exp_ = 'e' >> double_[ref(exp)=_1];

  bool r = boost::spirit::qi::phrase_parse(
    first, last,
    -char_('(') >> ( LHS_ || RHS_ ) >> -err_ >> -char_(')') >> -exp_,
    boost::spirit::ascii::space
  );

  int decimals = exponent(RHS) + zeroes;

  if (!pm && decimals && std::floor(err) == err)
    err = err * std::pow(10, -decimals);

  val = (double)LHS + (double)RHS * std::pow(10, -decimals);

  if (exp) {
    val = val * std::pow(10, exp);
    err = err * std::pow(10, exp);
  }

  return r;
}
