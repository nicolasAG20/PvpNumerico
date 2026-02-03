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


  allNumbers+= new Number(8,1,2,0, true)
  allNumbers+= new Number(5,2,0,1, false )
  allNumbers+= new Number(4,4,2,2,false)
  allNumbers+= new Number(6,1,2,3,true)
  allNumbers+= new Number(8,3,0,4,false)
  allNumbers+= new Number(5,5,0,5,false)
  allNumbers+= new Number(3,2,5,6,true)
  allNumbers+= new Number(4,3,0,7,false)
  allNumbers+= new Number(2,2,6,8,true)
  allNumbers+= new Number(5,6,1,9, false)

  class Team(numberBuffer: ArrayBuffer[Int]) {
    val teamName : String = numberBuffer.mkString
    protected var teamHp: Int = 0
    protected var teamAtk : Int = 0
    protected var teamMp : Int = 0
    protected var tHabilities : List[Hability]

    def setTeam(): Unit={
      var TNumbers = ArrayBuffer[Number]()
      for(number<- numberBuffer)
        for(i <- allNumbers)
          if(i.id == number){
            TNumbers += i

          }

    }


  }




}
