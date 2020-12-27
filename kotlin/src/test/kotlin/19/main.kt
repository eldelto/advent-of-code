package net.eldelto.aof.`2020`.`19`

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test


class FSMTest {

  @Test
  fun testRuneMachine() {
    data class Row(val machine: StateMachine, val events: List<Event>, val wantAccepting: Boolean)

    listOf(
      Row(newRuneMachine("a"), listOf(), false),
      Row(newRuneMachine("a"), listOf("a"), true),
      Row(newRuneMachine("a"), listOf("b"), false),
      Row(newRuneMachine("a"), listOf("a", "b"), false),
      Row(newRuneMachine("a"), listOf("a", "a"), false),
    ).forEach { (m, events, wantAccepting) ->
      var machine = m
      for (event in events) {
        machine = machine.transition(event)
      }

      assertEquals(wantAccepting, machine.isAccepting())
    }
  }

  @Test
  fun testConcatMachine() {
    data class Row(val machine: ConcatMachine, val events: List<Event>, val wantAccepting: Boolean)

    listOf(
      Row(ConcatMachine(newRuneMachine("a")), listOf(), false),
      Row(ConcatMachine(newRuneMachine("a")), listOf("a"), true),
      Row(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a"), false),
      Row(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a", "b"), true),
      Row(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a", "a"), false),
      Row(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a", "b", "b"), false),
    ).forEach { (m, events, wantAccepting) ->
      var machine = m
      for (event in events) {
        machine = machine.transition(event)
      }

      assertEquals(wantAccepting, machine.isAccepting(), events.joinToString(" "))
    }
  }

  @Test
  fun testAlternateMachine() {
    data class Row(val machine: AlternateMachine, val events: List<Event>, val wantAccepting: Boolean)

    listOf(
      Row(AlternateMachine(newRuneMachine("a")), listOf(), false),
      Row(AlternateMachine(newRuneMachine("a")), listOf("a"), true),
      Row(AlternateMachine(newRuneMachine("a")), listOf("b"), false),
      Row(AlternateMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a"), true),
      Row(AlternateMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("b"), true),
      Row(AlternateMachine(newRuneMachine("a"), newRuneMachine("b")), listOf("a", "b"), false),
      Row(
        AlternateMachine(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), newRuneMachine("a")),
        listOf("a"),
        true
      ),
      Row(
        AlternateMachine(ConcatMachine(newRuneMachine("a"), newRuneMachine("b")), newRuneMachine("b")),
        listOf("a", "b"),
        true
      ),
    ).forEach { (m, events, wantAccepting) ->
      var machine = m
      for (event in events) {
        machine = machine.transition(event)
      }

      assertEquals(wantAccepting, machine.isAccepting(), events.joinToString(" "))
    }
  }

  @Test
  fun testMatching() {
    data class Row(val rules: String, val word: String, val wantMatch: Boolean)

    listOf(
      Row(
        """0: 1
		       1: 3 | 2 1
		       2: "a"
		       3: "b"""".trimIndent(), "b", true
      ),
      Row(
        """0: 1
		 1: 3 2 | 3 1 2
		 8: 42 | 42 8
		 42: 9 3
		 9: 2 27
		 27: 1 6
		 6: 2 2 | 2 14
		 2: "a"
		 3: "b"""".trimIndent(), "bbbbaaaa", true
      ),
      Row(
        """0: 1 2
		 1: 2 3 2 | 2 3
		 2: "a"
		 3: "b"""".trimIndent(), "aba", true
      ),
      Row(
        """0: 1 2
		 1: 2 3 2 | 2 3
		 2: "a"
		 3: "b"""".trimIndent(), "abaa", true
      ),
      Row(
        """0: 1 2
		 1: 2 3 | 3 2
		 2: "a"
		 3: "b"""".trimIndent(), "aba", true
      ),
      Row(
        """0: 1 2
		 1: 2 3 | 3 2
		 2: "a"
		 3: "b"""".trimIndent(), "baa", true
      ),
    ).forEach { (rules, word, wantMatch) ->
      val fsm = parseRules(rules)
      val gotMatch = matchesWord(word, fsm)

      assertEquals(wantMatch, gotMatch, word)
    }
  }
}
