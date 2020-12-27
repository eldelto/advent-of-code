package net.eldelto.aof.`2020`.`19`

interface Validator {
  fun consume(word: String): List<String>
}

data class RuneValidator(val rune: Char) : Validator {
  override fun consume(word: String): List<String> = if (word.first() == rune) listOf(word.drop(1)) else listOf()
}

data class ConcatValidator(val validators: List<Validator>) : Validator {

  constructor(vararg validators: Validator) : this(validators.toList())

  override fun consume(word: String): List<String> = validators.fold(listOf(word)) { w, v ->
    w.flatMap(v::consume)
  }
}

data class AlternateValidator(val validators: List<Validator>) : Validator {

  constructor(vararg validators: Validator) : this(validators.toList())

  override fun consume(word: String): List<String> = validators.flatMap { it.consume(word) }
}
