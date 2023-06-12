extension String {
     func numberString(from index: String.Index) -> String {

         var endIndex = self.index(after: index)

         while endIndex < self.endIndex {
             if self[endIndex].isNumber == false {
                 return String(self[index..<endIndex])
             }
             endIndex = self.index(after: endIndex)
         }
         return String(self[index...])
     }
}