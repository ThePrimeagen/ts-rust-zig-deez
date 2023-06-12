extension String {
     func identifier(from index: String.Index) -> String {
         var endIndex = self.index(after: index)

         while endIndex < self.endIndex {
             if self[endIndex].isValidIdentifier == false {
                 return String(self[index..<endIndex])
             }
             endIndex = self.index(after: endIndex)
         }
         return String(self[index...])
     }
}