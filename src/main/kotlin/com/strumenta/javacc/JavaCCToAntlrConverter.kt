@file:JvmName("JavaCCToAntlrConverter")
package com.strumenta.javacc

import org.javacc.parser.*
import java.io.File

private fun Expansion.rewrite(rulesDefinitions: List<NormalProduction>): Expansion {
    val rewrittenExpansion = when (this) {
        is Sequence -> {
            // A USELESS B -> A B
            // TODO:
            // A A* -> A*
            // A A+ -> A+
            // A A? -> A+
            val rewrittenUnits = this.units
                .map { it as Expansion }
                .map { it.rewrite(rulesDefinitions) }

            val filteredUnits = rewrittenUnits.filter { !it.filterOutExpansion(rulesDefinitions) }

            this.units = filteredUnits as MutableList<in Any>?
            val unnestedRewrite = if (filteredUnits.size == 1) filteredUnits.first() else this
            unnestedRewrite
        }
        is Choice -> {
            // (A | USELESS | B) -> (A | B)
            // (A) -> A
            // (A | B | epsilon) -> (A)?
            val lastChoice = this.choices.last() as Expansion
            val optional = lastChoice.filterOutExpansion(rulesDefinitions)

            val rewrittenChoices = this.choices
                .map { it as Expansion }
                .map { it.rewrite(rulesDefinitions) }

            val filteredChoices = rewrittenChoices.filter { !it.filterOutExpansion(rulesDefinitions) }
            this.choices = filteredChoices.toMutableList()

            val unnestedRewrite = if (filteredChoices.size == 1) filteredChoices.first() else this
            val optionalRewrite =  if (optional) return ZeroOrOne(Token(), unnestedRewrite).rewrite(rulesDefinitions) else unnestedRewrite
            optionalRewrite
        }
        is ZeroOrOne -> {
            // A?? -> A?
            // A*? -> A*
            // A+? -> A*
            this.expansion = this.expansion.rewrite(rulesDefinitions)
            when (this.expansion) {
                is ZeroOrOne -> ZeroOrOne(Token(), (this.expansion as ZeroOrOne).expansion).rewrite(rulesDefinitions)
                is ZeroOrMore -> ZeroOrMore(Token(), (this.expansion as ZeroOrMore).expansion).rewrite(rulesDefinitions)
                is OneOrMore -> ZeroOrMore(Token(), (this.expansion as OneOrMore).expansion).rewrite(rulesDefinitions)
                else -> this
            }
        }
        is ZeroOrMore -> {
            // A?* -> A*
            // A** -> A*
            // A+* -> A*
            this.expansion = this.expansion.rewrite(rulesDefinitions)
            when (this.expansion) {
                is ZeroOrOne -> ZeroOrMore(Token(), (this.expansion as ZeroOrOne).expansion).rewrite(rulesDefinitions)
                is ZeroOrMore -> ZeroOrMore(Token(), (this.expansion as ZeroOrMore).expansion).rewrite(rulesDefinitions)
                is OneOrMore -> ZeroOrMore(Token(), (this.expansion as OneOrMore).expansion).rewrite(rulesDefinitions)
                else -> this
            }
        }
        is OneOrMore -> {
            // A?+ -> A*
            // A*+ -> A*
            // A++ -> A+
            this.expansion = this.expansion.rewrite(rulesDefinitions)
            when (this.expansion) {
                is ZeroOrOne -> ZeroOrMore(Token(), (this.expansion as ZeroOrOne).expansion).rewrite(rulesDefinitions)
                is ZeroOrMore -> ZeroOrMore(Token(), (this.expansion as ZeroOrMore).expansion).rewrite(rulesDefinitions)
                is OneOrMore -> OneOrMore(Token(), (this.expansion as OneOrMore).expansion).rewrite(rulesDefinitions)
                else -> this
            }
        }
        is TryBlock -> {
            this.exp = this.exp.rewrite(rulesDefinitions)
            this.exp.parent = this
            this
        }
        is RJustName -> this
        is REndOfFile -> this
        is RStringLiteral -> this
        is NonTerminal -> this
        is Lookahead -> this
        is Action -> {
            // Sometimes a user assigns a variable in an action instead of using a non terminal
            // for example:
            // { refValue = SimpleIdentifier(); }
            // instead of just
            // refValue = SimpleIdentifier()
            // Or:
            // { return new SqlDropTable(pos, CompoundIdentifier(), tableExistenceCheck); }

            // Look for the construction of non terminals
            val nonTerminals = mutableListOf<NonTerminal>()
            for (i in 0..this.actionTokens.size - 1) {
                val actionToken = this.actionTokens.get(i).image
                if ((actionToken == "(")
                    && (i - 2 >= 0)
                    && (this.actionTokens.get(i - 2).image != "new")
                    && rulesDefinitions.any { rule -> rule.lhs == this.actionTokens.get(i - 1).image }) {
                    val nonTerminal = NonTerminal()
                    nonTerminal.setName(this.actionTokens.get(i - 1).image)
                    nonTerminals.add(nonTerminal)
                }
            }

            if (nonTerminals.isEmpty()) {
                return this;
            }

            val sequence = Sequence()
            sequence.units.addAll(nonTerminals)
            return sequence.rewrite(rulesDefinitions)
        }
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }

    rewrittenExpansion.parent = this.parent
    return rewrittenExpansion
}

private fun Expansion.toString(lexerDefinitions: LexerDefinitions, namesToUncapitalize: List<String>): String {
    return when (this) {
        is Sequence -> this.units.joinToString(separator = " ") { (it as Expansion).toString(lexerDefinitions, namesToUncapitalize) }
        is Choice -> {
            val topLevelChoice = this.parent is NormalProduction
            if (!topLevelChoice) {
                "(" + this.choices.joinToString(separator = " | ") { (it as Expansion).toString(lexerDefinitions, namesToUncapitalize) } + ")"
            } else {
                // We don't need to wrap top level choices in a parenthesis
                // And we can line break
                "\n\t" + this.choices.joinToString(separator = " \n\t| ") { (it as Expansion).toString(lexerDefinitions, namesToUncapitalize) } + "\n  "
            }
        }
        is RStringLiteral -> lexerDefinitions.ruleForImage(image)?.name ?: "\"$image\""
        is NonTerminal -> this.name.uncapitalize()
        is ZeroOrOne -> {
            val optionalToString = this.expansion.toString(lexerDefinitions, namesToUncapitalize)
            if (this.expansion is Sequence) {
                "(${optionalToString})?"
            } else {
                "${optionalToString}?"
            }
        }
        is ZeroOrMore -> {
            val optionalToString = this.expansion.toString(lexerDefinitions, namesToUncapitalize)
            if (this.expansion is Sequence) {
                "(${optionalToString})*"
            } else {
                "${optionalToString}*"
            }
        }
        is OneOrMore -> {
            val atLeastOnceToString = this.expansion.toString(lexerDefinitions, namesToUncapitalize)
            if (this.expansion is Sequence) {
                "(${atLeastOnceToString})+"
            } else {
                "${atLeastOnceToString}+"
            }
        }
        is TryBlock -> this.exp.toString(lexerDefinitions, namesToUncapitalize)
        is RJustName -> this.label
        is REndOfFile -> "EOF"
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }
}

private fun Expansion.hasOptional(rulesDefinitions: List<NormalProduction>): Boolean {
    return when (this) {
        is Sequence -> {
            if (this.units.any { it !is Expansion }) {
                throw UnsupportedOperationException("Sequence element is not an Expansion")
            }

            this.units.any{(it as Expansion).hasOptional(rulesDefinitions)}
        }
        is Lookahead -> false
        is Choice -> {
            if (this.choices.any { it !is Expansion }) {
                throw UnsupportedOperationException("Choice element is not an Expansion")
            }

            (this.choices.last() as Expansion).filterOutExpansion(rulesDefinitions)
        }
        is RStringLiteral -> false
        is Action -> false
        is NonTerminal -> false
        is ZeroOrOne -> this.expansion.hasOptional(rulesDefinitions)
        is ZeroOrMore -> this.expansion.hasOptional(rulesDefinitions)
        is OneOrMore -> this.expansion.hasOptional(rulesDefinitions)
        is TryBlock -> this.exp.hasOptional(rulesDefinitions)
        is RJustName -> false
        is REndOfFile -> false
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }
}

private fun Expansion.filterOutExpansion(rulesDefinitions: List<NormalProduction>): Boolean {
    return when (this) {
        is Sequence -> {
            if (this.units.any { it !is Expansion }) {
                throw UnsupportedOperationException("Sequence element is not an Expansion")
            }

            this.units.all { (it as Expansion).filterOutExpansion(rulesDefinitions) }
        }
        is Lookahead -> true
        is Choice -> {
            if (this.choices.any { it !is Expansion }) {
                throw UnsupportedOperationException("Choice element is not an Expansion")
            }

            this.choices.all { (it as Expansion).filterOutExpansion(rulesDefinitions) }
        }
        is RStringLiteral -> false
        is Action -> true
        is NonTerminal -> rulesDefinitions.find { rule -> rule.lhs == this.name }?.expansion?.filterOutExpansion(rulesDefinitions) ?: true
        is ZeroOrOne -> this.expansion.filterOutExpansion(rulesDefinitions)
        is ZeroOrMore -> this.expansion.filterOutExpansion(rulesDefinitions)
        is OneOrMore -> this.expansion.filterOutExpansion(rulesDefinitions)
        is TryBlock -> this.exp.filterOutExpansion(rulesDefinitions)
        is RJustName -> false
        is REndOfFile -> false
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }
}

private fun Any.regExpDescriptorProcess() : String {
    return when (this) {
        is SingleCharacter -> this.ch.escapeCharacterInSet()
        is CharacterRange -> "${this.left}-${this.right}"
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }
}

private fun Char.toRegExp(): String {
    if (this.code == 12) {
        return "\\f"
    }
    return when (this) {
        '\\' -> "\\\\"
        ' ' -> " "
        '\'' -> "\\'"
        '\r' -> "\\r"
        '\n' -> "\\n"
        '\t' -> "\\t"
        else -> if (this.isWhitespace() || this.isISOControl() || this.category == CharCategory.FORMAT) {
            return "\\u${String.format("%04X", this.code.toLong())}"
        } else {
            this.toString()
        }

    }
}

private fun Char.escapeCharacterInSet(): String {
    // The following escaped characters are interpreted as single special characters:
    // \n, \r, \b, \t, \f, \uXXXX, and \u{XXXXXX}. To get ] or \ you must escape them with \. To get - you must escape it with \ too, except for the case when - is the first or last character in the set.
    if (this.code == 12) {
        return "\\f"
    }

    return when (this) {
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\b' -> "\\b"
        '\t' -> "\\t"
        ']'  -> "\\]"
        '\\' -> "\\\\"
        '-'  -> "\\-"
        ' ' -> " "
        else -> if (this.isWhitespace() || this.isISOControl() || this.category == CharCategory.FORMAT) {
            return "\\u${String.format("%04X", this.code.toLong())}"
        } else {
            this.toString()
        }

    }
}

private fun String.toRegExp() = this.toCharArray().joinToString(separator = "") { it.toRegExp() }


private fun RegularExpression.tokenProcess() : String {
    return when (this) {
        is RCharacterList -> "${if (this.negated_list) "~" else ""}[" + this.descriptors.map { it!!.regExpDescriptorProcess() }.joinToString(separator = "") + "]"
        is RStringLiteral -> if (this.image == "'") "'\\''" else "'${this.image.toRegExp()}'"
        is RSequence -> {
            if (this.units.any { it !is RegularExpression }) {
                throw UnsupportedOperationException("Sequence element is not an RegularExpression")
            }
            this.units.joinToString(separator = " ") { (it as RegularExpression).tokenProcess() }
        }
        is RZeroOrMore -> "(${this.regexpr.tokenProcess()})*"
        is ROneOrMore -> "(${this.regexpr.tokenProcess()})+"
        is RZeroOrOne -> "(${this.regexpr.tokenProcess()})?"
        is RJustName -> this.label
        is RChoice -> {
            if (this.choices.any { it !is RegularExpression }) {
                throw UnsupportedOperationException("Sequence element is not an RegularExpression")
            }

            "(" + this.choices.map { (it as RegularExpression).tokenProcess() }.joinToString(separator = " | ") + ")"
        }
        else -> throw UnsupportedOperationException("Not sure: ${this.javaClass.simpleName}")
    }
}

const val DEFAULT_MODE_NAME = "DEFAULT"

private fun RegExprSpec.toRuleDefinition(lexerState:String, action: String? = null, nameOverride: String? = null) : RuleDefinition {
    val prefix = ""
    val name = prefix + (nameOverride ?: rexp.label)
    return RuleDefinition(name, rexp.tokenProcess(), action, fragment=this.rexp.private_rexp)
}

private fun generateParserDefinitions(name: String, rulesDefinitions: List<NormalProduction>, lexerDefinitions: LexerDefinitions) : ParserDefinitions {
    val parserDefinitions = ParserDefinitions(name)
    val namesToUncapitalize = rulesDefinitions.map { it.lhs }

    // Hardcoding the start rule
    val startRule = NormalProduction()
    startRule.setLhs("startRule")
    val stmtListExpansion = NonTerminal()
    stmtListExpansion.name = "sqlStmtList"
    startRule.expansion = stmtListExpansion
    val rulesDefinitionsWithStartRule = mutableListOf<NormalProduction>()
    rulesDefinitionsWithStartRule.add(startRule)
    rulesDefinitionsWithStartRule.addAll(rulesDefinitions)

    rulesDefinitionsWithStartRule.forEach {
        if (!((it.expansion == null) || (it.expansion.filterOutExpansion(rulesDefinitions)))) {
            if(it.expansion.hasOptional(rulesDefinitions)) {
                println(it.lhs)
            }
            val name = it.lhs.uncapitalize()
            val rewrittenExpansion = it.expansion.rewrite(rulesDefinitions)
            val body = rewrittenExpansion.toString(lexerDefinitions, namesToUncapitalize);
            val ruleDefinition = RuleDefinition(name, body, null)
            parserDefinitions.addRuleDefinition(ruleDefinition)
        }
    }

    return parserDefinitions
}

private fun String.uncapitalize(): String {
    return if (this.isNotEmpty() && this[0].isUpperCase()) {
        this[0].lowercaseChar() + this.substring(1)
    } else {
        this
    }
}

private fun generateLexerDefinitions(name: String, tokenDefinitions: List<TokenProduction>) : LexerDefinitions {
    val lexerDefinitions = LexerDefinitions(name)
    var popCounter = 0
    var pushCounter = 0
    var consumeCounter = 0
    var skipCounter = 0;
    tokenDefinitions.forEach {
        val lexStates = it.lexStates
        val kindImage = TokenProduction.kindImage[it.kind]
        when (kindImage) {
            "SPECIAL" -> {
                // Creates a special token that does not participate in parsing. Already described earlier.
                it.respecs.forEach {
                    lexStates.forEach { ls ->
                        val action = "channel(HIDDEN)" + if ((it.act != null) && (it.act.actionTokens[0].equals("popState"))) ", popMode"  else ""
                        lexerDefinitions.addRuleDefinition(ls, it.toRuleDefinition(ls, action)) }
                }
            }
            "MORE" -> {
                // Continue to whatever the next state is, taking the matched string along.
                // This string will be a prefix of the new matched string.
                // This means we are not going to another state but we need to consume the token
                it.respecs.forEach {
                    val action = "channel(HIDDEN)" + if (it.nextState != null) ", " + (if (it.nextState == DEFAULT_MODE_NAME) "popMode" else "pushMode(${it.nextState})") else ""
                    if (it.nextState != null) {
                        lexStates.forEach { ls ->
                            run {
                                val name = if (it.nextState == DEFAULT_MODE_NAME) "POP" + popCounter++ else "PUSH" + pushCounter++
                                lexerDefinitions.addRuleDefinition(ls, it.toRuleDefinition(ls, action, name))
                            }
                        }
                    } else {
                        lexStates.forEach { ls ->
                            run {
                                val name = "CONSUME" + consumeCounter++
                                lexerDefinitions.addRuleDefinition(ls, it.toRuleDefinition(ls, action, name))
                            }
                        }
                    }
                }
            }
            "TOKEN" -> {
                // Create a token using the matched string and send it to the parser (or any caller).
                it.respecs.forEach {
                    if (it.nextState != null) {
                        val action = if (it.nextState == DEFAULT_MODE_NAME) "popMode" else  "pushMode(${it.nextState})"
                        lexStates.forEach { ls -> lexerDefinitions.addRuleDefinition(ls, it.toRuleDefinition(ls, action)) }
                    } else {
                        lexStates.forEach { ls -> lexerDefinitions.addRuleDefinition(ls, it.toRuleDefinition(ls)) }
                    }
                }
            }
            "SKIP" -> {
                // Simply throw away the matched string (after executing any lexical action).
                it.respecs.forEach {
                    lexStates.forEach { ls ->
                        run {
                            val name = "SKIP" + skipCounter++
                            val ruleDefinition = it.toRuleDefinition(ls, "skip", name)
                            lexerDefinitions.addRuleDefinition(ls, ruleDefinition)
                        }
                    }
                }
            }
            else -> throw UnsupportedOperationException(kindImage)
        }
    }

    // This rule is implicit in JavaCC but not in ANTLR
    lexerDefinitions.addRuleDefinition(
        "DEFAULT",
        RuleDefinition("QUOTED_IDENTIFIER", "QUOTE IDENTIFIER QUOTE", null))

    for (i in 0..25) {
        val ruleDefinition = RuleDefinition(('A' + i).toString(), "[" + ('a' + i).toString() + ('A' + i).toString() + "]", null, fragment = true)
        lexerDefinitions.addRuleDefinition("DEFAULT", ruleDefinition)
    }

    return lexerDefinitions
}

fun JavaCCGrammar.convertToAntlr(name: String) : AntlrGrammar {
    val lexerDefinitions = generateLexerDefinitions("${name}Lexer", this.tokenRules)
    val parserDefinitions = generateParserDefinitions("${name}Parser", this.parserRules, lexerDefinitions)
    return AntlrGrammar(lexerDefinitions, parserDefinitions)
}

fun main(args: Array<String>) {
    if (args.size != 1) {
        System.err.println("Specify the name of the JavaCC grammar to load")
        return
    }
    val file = File(args[0])
    val grammarName = file.nameWithoutExtension.replaceFirstChar(Char::titlecase)

    val javaCCGrammar = loadJavaCCGrammar(file)
    val antlrGrammar = javaCCGrammar.convertToAntlr(grammarName)

    antlrGrammar.saveLexer(File("${grammarName}Lexer.g4"))
    antlrGrammar.saveParser(File("${grammarName}Parser.g4"))
}