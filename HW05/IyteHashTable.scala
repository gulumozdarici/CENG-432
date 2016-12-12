//Gülüm ÖZDARICI -- 200201003
class IyteHashTable(){
  val size = 10000;
  val dir: Array[Array[(String)]] = new Array[Array[(String)]] (size)
  var countBucketElements : Int = 0
   
  def set(key:String, value:String) :Unit={
    
    val index = hash(key)
    if(dir(index) == null){
      var loadedArray = new Array[(String)](size)
      this.dir(index) = loadedArray
      loadedArray(0) = value
      countBucketElements += 1    
    }else{
      solvingCollision(this.dir(index), value)
      countBucketElements +=1
    }
  }
  
  def solvingCollision(tempArray: Array[(String)], value:String):Unit= {
    var index : Int = 0
    //linear solving to bucket size growth
    while(tempArray(index) != null){
      index+=1
    }
    tempArray(index) = value
  }
     
  def get(key: String): String = {
    val index : Int = hash(key)
    var tempString : String = ""
    if(dir(index) == null){
      return null
    }else{
      var tempIndex: Int = 0
      while(dir(index)(tempIndex) != null){
        tempString += dir(index)(tempIndex)
        tempIndex += 1
      }
    }
    tempString
  }  
    
 private def hash(Key: String): Int = {
    var hash = 0;
    for (i <- 0 to Key.length() - 1) {
      hash = (31 * hash + Key.charAt(i)) % size;
      if (hash < 0)
        hash = 0
    }
    return hash
  } 
}

object IyteHashTable {
  def apply() : IyteHashTable = new IyteHashTable()
}
