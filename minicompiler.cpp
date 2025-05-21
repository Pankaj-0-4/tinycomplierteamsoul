#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <cctype>
#include <cstring>
#include <utility> // For std::move

using namespace std;

// Token types
enum class TokenType {
    VAR, PRINT, IF, ELSE, RETURN,
    ID, NUMBER, STRING,
    OP, COMPARE, ASSIGN,
    LPAREN, RPAREN, LBRACE, RBRACE, SEMI,
    END
};

// Token structure
struct Token {
    TokenType type;
    string value;
    int line;
    int column;

    Token(TokenType t, const string& v, int l, int c)
        : type(t), value(v), line(l), column(c) {}
};

// AST Node structure
struct ASTNode {
    string nodeType;
    vector<unique_ptr<ASTNode>> children;
    string value;
    int indentLevel = 0;

    ASTNode(string type, string val = "")
        : nodeType(move(type)), value(move(val)) {}

    void addChild(unique_ptr<ASTNode> child) {
        child->indentLevel = indentLevel + 1;
        children.push_back(move(child));
    }
};

// Compiler class
class MiniCompiler {
private:
    vector<Token> tokens;
    size_t currentTokenIndex = 0;
    map<string, string> symbolTable;
    vector<string> errors;
    unique_ptr<ASTNode> ast;

    // Helper functions
    void addError(const string& error) { errors.push_back(error); }
    void addToken(TokenType type, const string& value, int line, int column) { tokens.emplace_back(type, value, line, column); }
    void addSymbol(const string& key, const string& value) { symbolTable[key] = value; }
    bool symbolExists(const string& key) const { return symbolTable.find(key) != symbolTable.end(); }

    // Tokenizer
    void tokenize(const string& source);

    // Parser
    const Token& currentToken() const;
    void advance();
    int getPrecedence(const string& op) const;
    unique_ptr<ASTNode> parseExpression(int minPrec);
    unique_ptr<ASTNode> parsePrimary();
    unique_ptr<ASTNode> parseDeclaration();
    unique_ptr<ASTNode> parsePrint();
    unique_ptr<ASTNode> parseReturn();
    unique_ptr<ASTNode> parseIfElse();
    unique_ptr<ASTNode> parseProgramBlock();
    unique_ptr<ASTNode> parseProgram();

    // Semantic Analysis
    void semanticAnalysis(ASTNode* node);

    // Print functions
    void printTokens() const;
    void printErrors() const;
    void printAST(const ASTNode* node) const;
    void printASTJSON(const ASTNode* node, ostream& out, int indent) const;

public:
    void compile(const string& filename);
    bool hasErrors() const { return !errors.empty(); }
};

// Implementation

void MiniCompiler::compile(const string& filename) {
    ifstream file(filename);
    if (!file) {
        cerr << "Error: Could not open file '" << filename << "'" << endl;
        return;
    }

    string sourceCode((istreambuf_iterator<char>(file)),
                           istreambuf_iterator<char>());
    file.close();

    cout << "=== Source Code ===" << endl;
    cout << sourceCode << endl << endl;

    tokens.clear();
    currentTokenIndex = 0;
    symbolTable.clear();
    errors.clear();
    ast.reset();

    cout << "=== Lexical Analysis (Tokenization) ===" << endl;
    tokenize(sourceCode);
    printTokens();

    if (!errors.empty()) {
        printErrors();
        return;
    }

    cout << "\n=== Syntax Analysis (Parsing) ===" << endl;
    ast = parseProgram();

    if (!errors.empty()) {
        printErrors();
        return;
    }

    cout << "\nParse Tree:" << endl;
    if (ast) printAST(ast.get());

    cout << "\nParse Tree (JSON):" << endl;
    if (ast) printASTJSON(ast.get(), cout, 0);
    cout << endl;

    cout << "\n=== Semantic Analysis ===" << endl;
    semanticAnalysis(ast.get());

    if (!errors.empty()) {
        printErrors();
        return;
    }

    cout << "\nSymbol Table:" << endl;
    for (const auto& entry : symbolTable) {
        cout << entry.first << ": " << entry.second << endl;
    }

    cout << "\nCompilation successful!" << endl;
}

void MiniCompiler::tokenize(const string& source) {
    size_t pos = 0;
    int line = 1;
    int lineStart = 0;
    size_t sourceLen = source.length();

    while (pos < sourceLen) {
        while (pos < sourceLen && isspace(source[pos])) {
            if (source[pos] == '\n') {
                line++;
                lineStart = pos + 1;
            }
            pos++;
        }
        if (pos >= sourceLen) break;

        int tokenStart = pos;
        TokenType type = TokenType::END;
        string value;

        if (isalpha(source[pos]) || source[pos] == '_') {
            while (pos < sourceLen && (isalnum(source[pos]) || source[pos] == '_')) {
                pos++;
            }
            value = source.substr(tokenStart, pos - tokenStart);

            if (value == "var") type = TokenType::VAR;
            else if (value == "print") type = TokenType::PRINT;
            else if (value == "if") type = TokenType::IF;
            else if (value == "else") type = TokenType::ELSE;
            else if (value == "return") type = TokenType::RETURN;
            else type = TokenType::ID;
        }
        else if (isdigit(source[pos])) {
            while (pos < sourceLen && isdigit(source[pos])) {
                pos++;
            }
            value = source.substr(tokenStart, pos - tokenStart);
            type = TokenType::NUMBER;
        }
        else if (source[pos] == '"') {
            pos++;
            tokenStart = pos;
            while (pos < sourceLen && source[pos] != '"') {
                pos++;
            }
            if (pos >= sourceLen) {
                addError("Unterminated string literal");
                break;
            }
            value = source.substr(tokenStart, pos - tokenStart);
            type = TokenType::STRING;
            pos++;
        }
        else if (strchr("+-*/", source[pos])) {
            value = source.substr(pos, 1);
            pos++;
            type = TokenType::OP;
        }
        else if (strchr("=!<>", source[pos])) {
            if (pos + 1 < sourceLen && source[pos+1] == '=') {
                value = source.substr(pos, 2);
                pos += 2;
                type = TokenType::COMPARE;
            } else {
                value = source.substr(pos, 1);
                pos++;
                type = (value == "=") ? TokenType::ASSIGN : TokenType::COMPARE;
            }
        }
        else {
            switch (source[pos]) {
                case '=': type = TokenType::ASSIGN; value = source.substr(pos, 1); pos++; break;
                case '(': type = TokenType::LPAREN; value = source.substr(pos, 1); pos++; break;
                case ')': type = TokenType::RPAREN; value = source.substr(pos, 1); pos++; break;
                case '{': type = TokenType::LBRACE; value = source.substr(pos, 1); pos++; break;
                case '}': type = TokenType::RBRACE; value = source.substr(pos, 1); pos++; break;
                case ';': type = TokenType::SEMI; value = source.substr(pos, 1); pos++; break;
                default: {
                    string error = "Illegal character '\\x";
                    char buf[3];
                    snprintf(buf, sizeof(buf), "%02x", (unsigned char)source[pos]);
                    error += buf;
                    error += "' at line " + to_string(line) +
                             ", column " + to_string(pos - lineStart);
                    addError(error);
                    pos++;
                    continue;
                }
            }
        }

        addToken(type, value, line, tokenStart - lineStart);
    }
    addToken(TokenType::END, "", line, 0);
}

const Token& MiniCompiler::currentToken() const {
    static const Token endToken{TokenType::END, "", 0, 0};
    return (currentTokenIndex < tokens.size()) ? tokens[currentTokenIndex] : endToken;
}

void MiniCompiler::advance() {
    if (currentTokenIndex < tokens.size() - 1) {
        currentTokenIndex++;
    } else {
        currentTokenIndex = tokens.size();
    }
}

int MiniCompiler::getPrecedence(const string& op) const {
    if (op.empty()) return -1;
    if (op == "+" || op == "-") return 1;
    if (op == "*" || op == "/") return 2;
    if (op == "<" || op == ">" || op == "<=" || op == ">=" || op == "==" || op == "!=") return 0;
    return -1;
}

unique_ptr<ASTNode> MiniCompiler::parsePrimary() {
    const Token& token = currentToken();
    unique_ptr<ASTNode> node;

    if (token.type == TokenType::ID) {
        node = make_unique<ASTNode>("Identifier", token.value);
        advance();
    }
    else if (token.type == TokenType::NUMBER) {
        node = make_unique<ASTNode>("NumberLiteral", token.value);
        advance();
    }
    else if (token.type == TokenType::LPAREN) {
        advance();
        node = parseExpression(0);
        if (!node) return nullptr;
        if (currentToken().type != TokenType::RPAREN) {
            addError("Expected ')' after expression");
            return nullptr;
        }
        advance();
    }
    else {
        addError("Expected identifier, number, or '('");
        return nullptr;
    }
    return node;
}

unique_ptr<ASTNode> MiniCompiler::parseExpression(int minPrec) {
    auto left = parsePrimary();
    if (!left) return nullptr;

    while (true) {
        const Token& token = currentToken();
        if (token.type != TokenType::OP && token.type != TokenType::COMPARE) break;

        string op = token.value;
        int prec = getPrecedence(op);
        if (prec < minPrec) break;

        advance();
        auto right = parseExpression(prec + 1);
        if (!right) return nullptr;

        auto bin = make_unique<ASTNode>("BinaryExpr", op);
        bin->addChild(move(left));
        bin->addChild(move(right));
        left = move(bin);
    }
    return left;
}

unique_ptr<ASTNode> MiniCompiler::parseDeclaration() {
    advance(); // skip 'var'
    if (currentToken().type != TokenType::ID) {
        addError("Expected identifier after 'var'");
        return nullptr;
    }

    string varName = currentToken().value;
    advance();
    unique_ptr<ASTNode> expr;

    if (currentToken().type == TokenType::ASSIGN) {
        advance();
        expr = parseExpression(0);
        if (!expr) {
            addError("Invalid expression in declaration");
            return nullptr;
        }
    }

    if (currentToken().type != TokenType::SEMI) {
        addError("Expected ';' at end of declaration");
        return nullptr;
    }
    advance();

    auto decl = make_unique<ASTNode>("Declaration", varName);
    if (expr) decl->addChild(move(expr));
    return decl;
}

unique_ptr<ASTNode> MiniCompiler::parsePrint() {
    advance();
    auto expr = parseExpression(0);
    if (!expr) {
        addError("Invalid expression in print");
        return nullptr;
    }

    if (currentToken().type != TokenType::SEMI) {
        addError("Expected ';' after print");
        return nullptr;
    }
    advance();

    auto node = make_unique<ASTNode>("Print");
    node->addChild(move(expr));
    return node;
}

unique_ptr<ASTNode> MiniCompiler::parseReturn() {
    advance();
    auto expr = parseExpression(0);
    if (!expr) {
        addError("Invalid expression in return");
        return nullptr;
    }
    if (currentToken().type != TokenType::SEMI) {
        addError("Expected ';' after 'return'");
        return nullptr;
    }
    advance();

    auto node = make_unique<ASTNode>("Return");
    node->addChild(move(expr));
    return node;
}

unique_ptr<ASTNode> MiniCompiler::parseIfElse() {
    advance();
    if (currentToken().type != TokenType::LPAREN) {
        addError("Expected '(' after 'if'");
        return nullptr;
    }
    advance();

    auto cond = parseExpression(0);
    if (!cond) {
        addError("Invalid condition in if");
        return nullptr;
    }
    if (currentToken().type != TokenType::RPAREN) {
        addError("Expected ')' after condition");
        return nullptr;
    }
    advance();

    if (currentToken().type != TokenType::LBRACE) {
        addError("Expected '{' after 'if' condition");
        return nullptr;
    }
    advance();

    auto ifBlock = parseProgramBlock();
    if (!ifBlock) return nullptr;

    if (currentToken().type == TokenType::ELSE) {
        advance();
        if (currentToken().type != TokenType::LBRACE) {
            addError("Expected '{' after 'else'");
            return nullptr;
        }
        advance();

        auto elseBlock = parseProgramBlock();
        if (!elseBlock) return nullptr;

        auto ifNode = make_unique<ASTNode>("IfElse");
        ifNode->addChild(move(cond));
        ifNode->addChild(move(ifBlock));
        ifNode->addChild(move(elseBlock));
        return ifNode;
    } else {
        auto ifNode = make_unique<ASTNode>("If");
        ifNode->addChild(move(cond));
        ifNode->addChild(move(ifBlock));
        return ifNode;
    }
}

unique_ptr<ASTNode> MiniCompiler::parseProgramBlock() {
    auto block = make_unique<ASTNode>("Block");
    while (currentToken().type != TokenType::RBRACE && currentToken().type != TokenType::END) {
        switch (currentToken().type) {
            case TokenType::VAR:
                if (auto decl = parseDeclaration()) block->addChild(move(decl));
                break;
            case TokenType::PRINT:
                if (auto prt = parsePrint()) block->addChild(move(prt));
                break;
            case TokenType::IF:
                if (auto ifstmt = parseIfElse()) block->addChild(move(ifstmt));
                break;
            case TokenType::RETURN:
                if (auto ret = parseReturn()) block->addChild(move(ret));
                break;
            default:
                addError("Unexpected token in block");
                advance();
                break;
        }
    }
    if (currentToken().type == TokenType::RBRACE) advance();
    return block;
}

unique_ptr<ASTNode> MiniCompiler::parseProgram() {
    auto program = make_unique<ASTNode>("Program");
    while (currentToken().type != TokenType::END) {
        switch (currentToken().type) {
            case TokenType::VAR:
                if (auto decl = parseDeclaration()) program->addChild(move(decl));
                break;
            case TokenType::PRINT:
                if (auto prt = parsePrint()) program->addChild(move(prt));
                break;
            case TokenType::IF:
                if (auto ifstmt = parseIfElse()) program->addChild(move(ifstmt));
                break;
            case TokenType::RETURN:
                if (auto ret = parseReturn()) program->addChild(move(ret));
                break;
            default:
                addError("Unexpected token in program");
                advance();
                break;
        }
    }
    return program;
}

void MiniCompiler::semanticAnalysis(ASTNode* node) {
    if (!node) return;

    if (node->nodeType == "Program" || node->nodeType == "Block") {
        for (const auto& child : node->children) {
            semanticAnalysis(child.get());
        }
    }
    else if (node->nodeType == "Declaration") {
        if (symbolExists(node->value)) {
            addError("Variable '" + node->value + "' already declared.");
        } else {
            addSymbol(node->value, "variable");
        }
        if (!node->children.empty()) {
            semanticAnalysis(node->children[0].get());
        }
    }
    else if (node->nodeType == "Identifier") {
        if (!symbolExists(node->value)) {
            addError("Undeclared variable '" + node->value + "'");
        }
    }
    else if (node->nodeType == "BinaryExpr") {
        semanticAnalysis(node->children[0].get());
        semanticAnalysis(node->children[1].get());
    }
    else if (node->nodeType == "If" || node->nodeType == "IfElse") {
        for (const auto& child : node->children) {
            semanticAnalysis(child.get());
        }
    }
    else if (node->nodeType == "Print" || node->nodeType == "Return") {
        for (const auto& child : node->children) {
            semanticAnalysis(child.get());
        }
    }
}

void MiniCompiler::printTokens() const {
    for (const auto& token : tokens) {
        cout << "Line " << token.line << ", Column " << token.column << ": ";
        switch (token.type) {
            case TokenType::VAR: cout << "VAR"; break;
            case TokenType::PRINT: cout << "PRINT"; break;
            case TokenType::IF: cout << "IF"; break;
            case TokenType::ELSE: cout << "ELSE"; break;
            case TokenType::RETURN: cout << "RETURN"; break;
            case TokenType::ID: cout << "ID"; break;
            case TokenType::NUMBER: cout << "NUMBER"; break;
            case TokenType::STRING: cout << "STRING"; break;
            case TokenType::OP: cout << "OP"; break;
            case TokenType::COMPARE: cout << "COMPARE"; break;
            case TokenType::ASSIGN: cout << "ASSIGN"; break;
            case TokenType::LPAREN: cout << "LPAREN"; break;
            case TokenType::RPAREN: cout << "RPAREN"; break;
            case TokenType::LBRACE: cout << "LBRACE"; break;
            case TokenType::RBRACE: cout << "RBRACE"; break;
            case TokenType::SEMI: cout << "SEMI"; break;
            case TokenType::END: cout << "END"; break;
        }
        cout << " = " << token.value << endl;
    }
}

void MiniCompiler::printErrors() const {
    cout << "\nCompilation errors:" << endl;
    for (const auto& error : errors) {
        cout << error << endl;
    }
}

void MiniCompiler::printAST(const ASTNode* node) const {
    if (!node) return;
    cout << string(node->indentLevel * 2, ' ') << node->nodeType;
    if (!node->value.empty()) cout << " (" << node->value << ")";
    cout << endl;
    for (const auto& child : node->children) {
        printAST(child.get());
    }
}

void MiniCompiler::printASTJSON(const ASTNode* node, ostream& out, int indent) const {
    if (!node) return;
    out << string(indent, ' ') << "{\n";
    out << string(indent + 2, ' ') << "\"type\": \"" << node->nodeType << "\"";
    if (!node->value.empty()) {
        out << ",\n" << string(indent + 2, ' ') << "\"value\": \"" << node->value << "\"";
    }
    if (!node->children.empty()) {
        out << ",\n" << string(indent + 2, ' ') << "\"children\": [\n";
        for (size_t i = 0; i < node->children.size(); i++) {
            printASTJSON(node->children[i].get(), out, indent + 4);
            if (i + 1 < node->children.size()) out << ",\n";
        }
        out << "\n" << string(indent + 2, ' ') << "]";
    }
    out << "\n" << string(indent, ' ') << "}";
}

// Main function
int main(int argc, char* argv[]) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <filename.mini>" << endl;
        return 1;
    }

    MiniCompiler compiler;
    compiler.compile(argv[1]);

    return compiler.hasErrors() ? 1 : 0;
}