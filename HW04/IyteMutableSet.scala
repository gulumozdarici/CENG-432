      //Gülüm ÖZDARICI 200201003

class IyteMutableSet(){  
   class TreeNode(x: Int){
      var left : TreeNode = null
      var right : TreeNode = null
      var value: Int = x
   }
  
    var Root : TreeNode = null
    
    def add(x: Int) {
      insert(x, Root)
    }

    def contains (x: Int): Boolean={
      search(x, Root)
    }

    def insert (x: Int, node: TreeNode) : Unit={
      var temp : TreeNode = node
      
      if(temp == null){
        Root = new TreeNode(x)
      }
      else if(temp.value >= x){
        if(temp.left != null){
          insert(x, temp.left)
      }
        else temp.left = new TreeNode(x)
      }
      else if(temp.value < x){
        if(temp.right != null){
          insert(x, temp.right)
      }
      else temp.right = new TreeNode(x)
        
      }
    }
    
    def search(x: Int, node: TreeNode): Boolean ={
      var temp : TreeNode = node
      if (node == null) false
      else if(temp.value == x) true
      else if(temp.value > x ){
        search(x, temp.left)
      }
      else{
        search(x, temp.right)
      }
      
    }
  
  
    override def toString() : String ={
      return preorderToString(Root)
    }



    def preorderToString( node: TreeNode): String ={
      var finalString : String = " "
      var temp : TreeNode = node
   
      if(node != null){
        finalString += temp.value
        finalString += ","
       // print(finalString)
        preorderToString(temp.left)
        preorderToString(temp.right)

        return finalString      
      }else if (finalString == null) {
      //  print(finalString)
        return finalString;
      }else{
        finalString.substring(0, finalString.length-1)
      }
    }


   }
  object IyteMutableSet{
    def apply() : IyteMutableSet = new IyteMutableSet()
  }