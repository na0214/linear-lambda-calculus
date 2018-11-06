# Linear Lambda Calculus

OCamlによる、線形ラムダ計算の型システムの実装です。
以下のような構文を持ちます

```
<qualifier> ::= lin | un
<boolean> ::= true | false
<term> ::= identifier
        |  <qualifier> <boolean>
        | if <term> then <term> else <term>
        | <qualifier> '<' <term> ',' <term> '>'
        | split <term> as identifier ',' identifier in <term>
        | <qualifier> '#' identifier ':' <qualified-type> '.' <term>
        | <term> <term>
<qualified-type> ::= <qualifier> <type>
<type> ::= Bool
        |  <qualified-type> '*' <qualified-type>
        |  <qualified-type> '->' <qualified-type>
```
