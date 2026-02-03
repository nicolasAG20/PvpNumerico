import scala.collection.mutable.ArrayBuffer
object BattleField extends App {

  class Hability(nHeals: Boolean) {
    var heals: Boolean = nHeals

    def useHability(atkValue: Int, mpValue: Int): Int = {
      if (heals) {
        (mpValue * 1.5).toInt
      } else {
        (atkValue * 1.5).toInt
      }

    }
  }

  class Number(nHp: Int, nAtk: Int, nMp: Int, nId: Int, nHeals: Boolean) {
    val hp: Int = nHp
    val atk: Int = nAtk
    val mp: Int = nMp
    val id: Int = nId
    val hability: Hability = new Hability(nHeals)
  }

  var allNumbers = ArrayBuffer[Number]()


  allNumbers += new Number(16, 1, 2, 0, true)
  allNumbers += new Number(10, 2, 0, 1, false)
  allNumbers += new Number(8, 4, 2, 2, false)
  allNumbers += new Number(12, 1, 2, 3, true)
  allNumbers += new Number(16, 3, 0, 4, false)
  allNumbers += new Number(10, 5, 0, 5, false)
  allNumbers += new Number(6, 2, 5, 6, true)
  allNumbers += new Number(8, 3, 0, 7, false)
  allNumbers += new Number(4, 2, 6, 8, true)
  allNumbers += new Number(10, 6, 1, 9, false)

  class Team(numberBuffer: ArrayBuffer[Int], operator: String) {
    val teamName: String = numberBuffer.mkString
    var teamHp: Int = 0 // volver protected despues
    protected var alive: Boolean = true //boleano que se verifica cada ronda para ver si algun equipo sigue vivo
    protected var maxTeamHp: Int = 0
    protected var teamAtk: Int = 0
    protected var teamMp: Int = 0
    protected var tHabilities = ArrayBuffer[Hability]()

    setTeam()

    def setTeam(): Unit = {

      for (number <- numberBuffer) {

        for (i <- allNumbers) {

          if (i.id == number) {
            teamHp += i.hp
            teamAtk += i.atk
            teamMp += i.mp
            tHabilities += i.hability
          }
        }
      }
      maxTeamHp = teamHp

      if (operator == "-") {
        val lossHp = numberBuffer.length * 1
        teamHp -= lossHp
        if (teamHp <= 0) {
          teamHp = 1
        }
      }
    }

    def modifyHp(modiffier: Int): Unit = {
      teamHp += modiffier
      if (teamHp <= 0) alive = false
    }

    def useAction(enemyTeam: Team): Unit = {
      val rand = new scala.util.Random
      val randNumb = rand.nextInt(tHabilities.length)
      val habSelected = tHabilities(randNumb)
      var habValue: Int = 0

      if (habSelected.heals == true) {
        habValue = habSelected.useHability(teamAtk, teamMp)
        if ((habValue + teamHp) >= maxTeamHp) {
          teamHp = maxTeamHp
        } else {
          teamHp += habValue
        }

      } else {
        habValue = -1 * (habSelected.useHability(teamAtk, teamMp)) // valor negativo pq hace daño
        enemyTeam.modifyHp(habValue)
      }
    }

    def isAlive: Boolean = alive

  }

  class Battle(team1: Team, team2: Team) {
    def start(): Unit = {
      var round = 1
      while (team1.isAlive && team2.isAlive) {
        println(s"---Ronda $round ---")
        team1.useAction(team2)
        if (team2.isAlive)
          team2.useAction(team1)
        Graficador.mostrarEstado(team1, team2)
        round += 1
      }

      if (team1.isAlive)
        println(s"Gana el equipo ${team1.teamName}")
      else
        println(s"Gana el equipo ${team2.teamName}")
    }
  }

  class BattleThread(battle: Battle) extends Thread {
    override def run(): Unit = {
      battle.start()
    }
  }

  object Graficador {
    def mostrarEstado(team1: Team, team2: Team): Unit = {
      println(s"${team1.teamName} HP:${team1.teamHp}")
      println(s"${team2.teamName} HP:${team2.teamHp}")
    }
  }

  def startBattle(): Unit = {
    println("Ingrese la operacion (ej: 125+256): ")
    val input = scala.io.StdIn.readLine()

    var operator = ""
    if (input.contains("+")) {
      operator = "+"
    } else if (input.contains("-")) {
      operator = "-"
    } else {
      println("Operacion invalida")
      System.exit(0)
    }

    val parts = input.split("\\" + operator)
    val leftTeam = parts(0)
    val rightTeam = parts(1)

    val team1List = leftTeam.map(_.asDigit).toBuffer
    val team2List = rightTeam.map(_.asDigit).toBuffer

    val team1 = new Team(ArrayBuffer.from(team1List), operator)
    val team2 = new Team(ArrayBuffer.from(team2List), operator)

    val battle = new Battle(team1, team2)
    val thread = new BattleThread(battle)
    thread.start()
    thread.join()
  }

  var opcion = 0
  while (opcion != 2) {
    println("1) Nueva batalla")
    println("2) Salir")
    print("Opcion: ")

    opcion = scala.io.StdIn.readInt()

    opcion match {
      case 1 =>
        startBattle()

      case 2 =>
        println("Saliendo del juego...")

      case _ =>
        println("Opcion invalida")
    }
  }
}
