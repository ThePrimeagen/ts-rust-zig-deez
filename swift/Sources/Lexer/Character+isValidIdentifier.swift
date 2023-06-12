extension Character {
     var isValidIdentifier: Bool {
         self.isLetter || self == "_"
     }
}