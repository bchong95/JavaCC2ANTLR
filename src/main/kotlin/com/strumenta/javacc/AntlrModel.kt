package com.strumenta.javacc

import org.snt.inmemantlr.GenericParser
import java.io.File
import java.io.PrintWriter
import java.io.StringWriter
import java.util.*

data class RuleDefinition(val name: String, val body: String, val action: String?, val fragment: Boolean = false) {
    fun generate() : String {
        val prefix = if (fragment) "fragment " else ""
        val actionPostfix = if (action == null) "" else "-> $action"
        val body= if(action!=null && body.contains("|")) "($body)" else body
        return "$prefix$name : $body $actionPostfix ;"
    }
}

class ParserDefinitions(val name: String) {

    private val rules = LinkedList<RuleDefinition>()

    fun generate(lexerName: String? = null): String {
        val stringWriter = StringWriter()
        val printWriter = PrintWriter(stringWriter)
        printWriter.println("parser grammar $name;")
        if (lexerName != null) {
            printWriter.println()
            printWriter.println("options { tokenVocab=$lexerName; }")
        }
        rules.forEach {
            printWriter.println()

            printWriter.println(it.generate())
        }
        return stringWriter.toString()
    }

    fun addRuleDefinition(ruleDefinition: RuleDefinition) {
        rules.add(ruleDefinition)
    }
}

class LexerDefinitions(val name: String) {

    private val rulesByMode  = HashMap<String, MutableList<RuleDefinition>>()

    fun ruleForImage(image: String, mode: String = DEFAULT_MODE_NAME) : RuleDefinition? {
        return rulesByMode[mode]?.firstOrNull {
            it.body == "'$image'"
        }
    }

    fun addRuleDefinition(mode: String, ruleDefinition: RuleDefinition) {
        if ((ruleDefinition.name.length == 1) and (ruleDefinition.body.length == 3)) {
            // We create fragments for all the single letter tokens later.
            return;
        }

        if (!rulesByMode.containsKey(mode)) {
            rulesByMode[mode] = LinkedList()
        }
        var ruleDefinitionCorrected = ruleDefinition
        if (ruleDefinition.name.isEmpty()) {
            if (rulesByMode[mode]!!.any { it.body == ruleDefinition.body }) {
                return
            }
            ruleDefinitionCorrected = ruleDefinition.copy(name = generateName(ruleDefinition.body, rulesByMode[mode]!!.map { it.name }.toSet()))
        }

        if (ruleDefinitionCorrected.name.startsWith("_")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(name = "UNDERSCORE${ruleDefinitionCorrected.name}")
        }
        if (ruleDefinitionCorrected.name == ruleDefinitionCorrected.body) {
            return
        }
        if (ruleDefinitionCorrected.body.contains("~[]")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("~[]", "."))
        }
        if (ruleDefinitionCorrected.body.contains("_NUM_CHAR")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_NUM_CHAR", "UNDERSCORE_NUM_CHAR"))
        }
        if (ruleDefinitionCorrected.body.contains("_ESCAPED_CHAR")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_ESCAPED_CHAR", "UNDERSCORE_ESCAPED_CHAR"))
        }
        if (ruleDefinitionCorrected.body.contains("_TERM_START_CHAR")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_TERM_START_CHAR", "UNDERSCORE_TERM_START_CHAR"))
        }
        if (ruleDefinitionCorrected.body.contains("_TERM_CHAR")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_TERM_CHAR", "UNDERSCORE_TERM_CHAR"))
        }
        if (ruleDefinitionCorrected.body.contains("_WHITESPACE")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_WHITESPACE", "UNDERSCORE_WHITESPACE"))
        }
        if (ruleDefinitionCorrected.body.contains("_QUOTED_CHAR")) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body.replace("_QUOTED_CHAR", "UNDERSCORE_QUOTED_CHAR"))
        }
        if(ruleDefinitionCorrected.body.startsWith("'")
            and ruleDefinitionCorrected.body.endsWith("'")
            and ruleDefinition.body.subSequence(1, ruleDefinition.body.length -1).all {(it in 'a'..'z') or (it in 'A'..'Z') or (it == '_')}) {
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = ruleDefinitionCorrected.body
                .filter { it != '\'' }
                .map { it -> if ((it in 'a'..'z') or (it in 'A'..'Z')) it.uppercase()  else "'$it'" }
                .joinToString(" "))
        }

        if (ruleDefinitionCorrected.name == "BRACKET_QUOTED_IDENTIFIER") {
            // This rule has JAVA code of escaping the identifier
            ruleDefinitionCorrected = ruleDefinitionCorrected.copy(body = "'[' IDENTIFIER ']'")
        }

        rulesByMode[mode]!!.add(ruleDefinitionCorrected)
    }

    private fun generateName(body: String, usedNames: Set<String>) : String {
        throw UnsupportedOperationException(body)
    }

    fun generate() : String {
        val stringWriter = StringWriter()
        val printWriter = PrintWriter(stringWriter)
        printWriter.println("lexer grammar $name;")
        printMode(DEFAULT_MODE_NAME, printWriter)
        rulesByMode.keys.filter { it != DEFAULT_MODE_NAME }.forEach { printMode(it, printWriter) }
        return stringWriter.toString()
    }

    private fun printMode(mode: String, printWriter: PrintWriter) {
        printWriter.println()
        if (mode != DEFAULT_MODE_NAME) {
            printWriter.println("mode $mode;")
        }

        rulesByMode[mode]!!.forEach {
            // If a rule is already covered in default mode then we need to handle it like so:
            /*
            WS : [ \t]+;
            RULE_WITH_ACTION: 'foo' -> skip
            mode Mode1;
                Mode1_WS : WS -> type(WS);
                Mode1_RULE_WITH_ACTION: RULE_WITH_ACTION -> skip
            mode Mode2;
                Mode2_WS : WS -> type(WS);
                Mode2_RULE_WITH_ACTION: RULE_WITH_ACTION -> skip
             */
            if (mode.contains("LUCENE")) {
                // Special rules for the lucene rules
                if ((mode != "LUCENE_DEFAULT") && (rulesByMode["LUCENE_DEFAULT"]!!.any{ ruleDefinition ->  ruleDefinition.name == it.name })) {
                    var action = when {
                        it.fragment -> null
                        it.action != null -> it.action
                        else -> "type(" + it.name + ")"
                    }
                    val ruleDefinition = RuleDefinition(mode + "_" + it.name, it.name, action)
                    printWriter.println(ruleDefinition.generate())
                } else {
                    printWriter.println(it.generate())
                }
            } else {
                if ((mode != DEFAULT_MODE_NAME) && (rulesByMode[DEFAULT_MODE_NAME]!!.any{ ruleDefinition ->  ruleDefinition.name == it.name })) {
                    var action = when {
                        it.fragment -> null
                        it.action != null -> it.action
                        else -> "type(" + it.name + ")"
                    }
                    val ruleDefinition = RuleDefinition(mode + "_" + it.name, it.name, action)
                    printWriter.println(ruleDefinition.generate())
                } else {
                    printWriter.println(it.generate())
                }
            }
        }
    }
}

class AntlrGrammar(private val lexerDefinitions: LexerDefinitions, private val parserDefinitions: ParserDefinitions) {
    private fun lexerCode() = lexerDefinitions.generate()
    private fun parserCode() = parserDefinitions.generate(lexerDefinitions.name)
    fun saveLexer(file: File) {
        file.printWriter().use { out -> out.print(lexerCode()) }
    }
    fun saveParser(file: File) {
        file.printWriter().use { out -> out.print(parserCode()) }
    }
    fun genericParser() : GenericParser {
        val genericParser = GenericParser(lexerCode(), parserCode())
        genericParser.compile()
        return genericParser
    }
}
