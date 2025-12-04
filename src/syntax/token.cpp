
#include "xylo/syntax/token.h"

#include "xylo/util/contract.h"

namespace xylo {

static_assert(sizeof(int64_t) == 8);
static_assert(sizeof(double) == 8);


const char* TokenName(Token token) {
  static const char** table = [] {
    static const char* t[TokenCount()];

#ifndef NDEBUG
    for (size_t i = 0; i < TokenCount(); ++i) {
      t[i] = nullptr;
    }
#endif

    t[static_cast<size_t>(Token::kNull)] = "null";
    t[static_cast<size_t>(Token::kError)] = "error";
    t[static_cast<size_t>(Token::kEOF)] = "eof";
    t[static_cast<size_t>(Token::kNewline)] = "newline";

    t[static_cast<size_t>(Token::kIdentifier)] = "identifier";
    t[static_cast<size_t>(Token::kInteger)] = "integer literal";
    t[static_cast<size_t>(Token::kFloat)] = "float literal";
    t[static_cast<size_t>(Token::kChar)] = "char literal";
    t[static_cast<size_t>(Token::kString)] = "string literal";

    t[static_cast<size_t>(Token::kAdd)] = "'+'";
    t[static_cast<size_t>(Token::kSub)] = "'-'";
    t[static_cast<size_t>(Token::kMul)] = "'*'";
    t[static_cast<size_t>(Token::kDiv)] = "'/'";
    t[static_cast<size_t>(Token::kRem)] = "'%'";

    t[static_cast<size_t>(Token::kAnd)] = "'&'";
    t[static_cast<size_t>(Token::kOr)] = "'|'";
    t[static_cast<size_t>(Token::kXor)] = "'^'";
    t[static_cast<size_t>(Token::kNot)] = "'~'";
    t[static_cast<size_t>(Token::kShl)] = "'<<'";
    t[static_cast<size_t>(Token::kShr)] = "'>>'";

    t[static_cast<size_t>(Token::kLAnd)] = "'&&'";
    t[static_cast<size_t>(Token::kLOr)] = "'||'";
    t[static_cast<size_t>(Token::kLNot)] = "'!'";

    t[static_cast<size_t>(Token::kEQ)] = "'=='";
    t[static_cast<size_t>(Token::kNE)] = "'!='";
    t[static_cast<size_t>(Token::kLT)] = "'<'";
    t[static_cast<size_t>(Token::kLE)] = "'<='";
    t[static_cast<size_t>(Token::kGT)] = "'>'";
    t[static_cast<size_t>(Token::kGE)] = "'>='";

    t[static_cast<size_t>(Token::kAssign)] = "'='";
    t[static_cast<size_t>(Token::kAddAssign)] = "'+='";
    t[static_cast<size_t>(Token::kSubAssign)] = "'-='";
    t[static_cast<size_t>(Token::kMulAssign)] = "'*='";
    t[static_cast<size_t>(Token::kDivAssign)] = "'/='";
    t[static_cast<size_t>(Token::kRemAssign)] = "'%='";
    t[static_cast<size_t>(Token::kAndAssign)] = "'&='";
    t[static_cast<size_t>(Token::kOrAssign)] = "'|='";
    t[static_cast<size_t>(Token::kXorAssign)] = "'^='";
    t[static_cast<size_t>(Token::kShlAssign)] = "'<<='";
    t[static_cast<size_t>(Token::kShrAssign)] = "'>>='";

    t[static_cast<size_t>(Token::kLParen)] = "'('";
    t[static_cast<size_t>(Token::kRParen)] = "')'";
    t[static_cast<size_t>(Token::kLBrack)] = "'['";
    t[static_cast<size_t>(Token::kRBrack)] = "']'";
    t[static_cast<size_t>(Token::kLBrace)] = "'{'";
    t[static_cast<size_t>(Token::kRBrace)] = "'}'";

    t[static_cast<size_t>(Token::kComma)] = "','";
    t[static_cast<size_t>(Token::kPeriod)] = "'.'";
    t[static_cast<size_t>(Token::kColon)] = "':'";
    t[static_cast<size_t>(Token::kSemicolon)] = "';'";
    t[static_cast<size_t>(Token::kQuestion)] = "'?'";
    t[static_cast<size_t>(Token::kArrow)] = "'->'";
    t[static_cast<size_t>(Token::kDoubleArrow)] = "'=>'";

    t[static_cast<size_t>(Token::kInterface)] = "'interface'";
    t[static_cast<size_t>(Token::kClass)] = "'class'";
    t[static_cast<size_t>(Token::kEmbed)] = "'embed'";
    t[static_cast<size_t>(Token::kDef)] = "'def'";
    t[static_cast<size_t>(Token::kFn)] = "'fn'";
    t[static_cast<size_t>(Token::kReturn)] = "'return'";
    t[static_cast<size_t>(Token::kLet)] = "'let'";
    t[static_cast<size_t>(Token::kVar)] = "'var'";
    t[static_cast<size_t>(Token::kIf)] = "'if'";
    t[static_cast<size_t>(Token::kElse)] = "'else'";
    t[static_cast<size_t>(Token::kNew)] = "'new'";
    t[static_cast<size_t>(Token::kTrue)] = "'true'";
    t[static_cast<size_t>(Token::kFalse)] = "'false'";
    t[static_cast<size_t>(Token::kThis)] = "'this'";

#ifndef NDEBUG
    for (size_t i = 0; i < TokenCount(); ++i) {
      xylo_contract(t[i] != nullptr);
    }
#endif

    return t;
  }();

  return table[static_cast<size_t>(token)];
}


}  // namespace xylo
