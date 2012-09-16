{
module Parser (parse) where
-- what is comment here
}

%name parse
%tokentype { Token }
%error { parseError }



{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
