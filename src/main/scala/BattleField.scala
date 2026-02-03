import scala.collection.mutable.ArrayBuffer
object BattleField extends App {

  class Hability(nHeals : Boolean) {
    var heals: Boolean  = nHeals

    def useHability(atkValue: Int, mpValue: Int): Int = {
      if(heals){
        (mpValue * 1.5).toInt
      }else{
        (atkValue * 1.5).toInt
      }

    }
  }





  class Number(nHp:Int , nAtk :Int ,nMp : Int, nId : Int, nHeals: Boolean) {
    val hp: Int = nHp
    val atk: Int = nAtk
    val mp : Int = nMp
    val id : Int = nId
    val hability : Hability = new Hability(nHeals)
  }

  var allNumbers = ArrayBuffer[Number]()


  allNumbers+= new Number(16,1,2,0, true)
  allNumbers+= new Number(10,2,0,1, false )
  allNumbers+= new Number(8,4,2,2,false)
  allNumbers+= new Number(12,1,2,3,true)
  allNumbers+= new Number(16,3,0,4,false)
  allNumbers+= new Number(10,5,0,5,false)
  allNumbers+= new Number(6,2,5,6,true)
  allNumbers+= new Number(8,3,0,7,false)
  allNumbers+= new Number(4,2,6,8,true)
  allNumbers+= new Number(10,6,1,9, false)

  class Team(numberBuffer: ArrayBuffer[Int]) {
    val teamName : String = numberBuffer.mkString
    var teamHp: Int = 0 // volver protected despues
    protected var alive : Boolean = true //boleano que se verifica cada ronda para ver si algun equipo sigue vivo
    protected  var maxTeamHp : Int=0
    protected var teamAtk : Int = 0
    protected var teamMp : Int = 0
    protected var tHabilities = ArrayBuffer[Hability]()

    setTeam()

    def setTeam(): Unit={

      for(number<- numberBuffer) {

        for(i <- allNumbers) {

          if(i.id == number){
            teamHp += i.hp
            teamAtk += i.atk
            teamMp += i.mp
            tHabilities += i.hability
          }
        }
      }
      maxTeamHp = teamHp
    }

    def modifyHp(modiffier: Int): Unit={
      teamHp += modiffier
      if(teamHp<=0) alive=false
    }

    def useAction(enemyTeam : Team ):Unit = {
      val rand = new scala.util.Random
      val randNumb = rand.nextInt(tHabilities.length)
      val habSelected = tHabilities(randNumb)
      var habValue: Int = 0
      if (habSelected.heals == true){
        habValue = habSelected.useHability(teamAtk, teamMp)
        if((habValue+teamHp)>= maxTeamHp){
          teamHp = maxTeamHp
        }else{
          teamHp += habValue
        }

      } else {
        habValue = -1 * (habSelected.useHability(teamAtk, teamMp)) // valor negativo pq hace daño
        enemyTeam.modifyHp(habValue)
      }
    }


  }

  //codigo para comprovar creación de equipos y testeo de daño

  /*var list1 = new ArrayBuffer[Int]()
  list1 += 1
  list1 += 2
  list1 += 3
  val list2 = list1.map(x=>x+1)
  var team1: Team = new Team(list1)
  var team2: Team = new Team(list2)
  println("vida " + team1.teamHp)
  team2.useAction(team1)
  println("vida " + team1.teamHp)
*/


}
