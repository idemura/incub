{- 1 {-22-} 3 -}

{template test where
    name :: ByteString
    bal :: Integer
    ops :: [Integer]
}

<html>
<head>
<title>West Templates</title>
</head>
<body>
  Hello {$name}! Your balance {$bal}.
  {if bal > 0}
    Thank you for using West!
  {elif bal < -1000}
    Service blocked. Contact <a href="/srv-stop">administator</a>.
  {else}
    Please <a href="/refund">refund</a> it.
  {if/}

  Operations:
  {for op <- ops}
    {$op}<br/>
  {for/}

</body>
</html>

{template/}
