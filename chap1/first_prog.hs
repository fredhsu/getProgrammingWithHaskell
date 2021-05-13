toPart recipient = "To " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying the book: " ++ bookTitle ++ ".\n"
fromPart author = "Sincerely,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

main :: IO()
main = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the Author?"
    author <- getLine
    print (createEmail recipient title author)

