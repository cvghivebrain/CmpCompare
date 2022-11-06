unit ExplodeFunc;

interface
uses Sysutils, StrUtils;

function Explode(str, delimiter: string; n: integer): string;
function ExplodeFull(str, delimiter: string; n: integer): string;

implementation

{ Replicate MediaWiki's "explode" string function. }

function Explode(str, delimiter: string; n: integer): string; // Get substring from string using delimiter.
begin
  if (AnsiPos(delimiter,str) = 0) and ((n = 0) or (n = -1)) then result := str // Output full string if delimiter not found.
  else
    begin
    str := str+delimiter;
    while n > 0 do
      begin
      Delete(str,1,AnsiPos(delimiter,str)+Length(delimiter)-1); // Trim earlier substrings and delimiters.
      dec(n);
      end;
    Delete(str,AnsiPos(delimiter,str),Length(str)-AnsiPos(delimiter,str)+1); // Trim later substrings and delimiters.
    result := str;
    end;
end;

{ Full "explode" function, with negative substrings (i.e. reading from the right). }

function ExplodeFull(str, delimiter: string; n: integer): string;
var n2: integer;
begin
  if (AnsiPos(delimiter,str) = 0) and ((n = 0) or (n = -1)) then result := str // Output full string if delimiter not found.
  else
    begin
    if n > -1 then // Check for negative substring.
      begin
      str := str+delimiter;
      n2 := n;
      end
    else
      begin
      delimiter := AnsiReverseString(delimiter);
      str := AnsiReverseString(str)+delimiter; // Reverse string for negative.
      n2 := Abs(n)-1;
      end;
    while n2 > 0 do
      begin
      Delete(str,1,AnsiPos(delimiter,str)+Length(delimiter)-1); // Trim earlier substrings and delimiters.
      dec(n2);
      end;
    Delete(str,AnsiPos(delimiter,str),Length(str)-AnsiPos(delimiter,str)+1); // Trim later substrings and delimiters.
    if n < 0 then str := AnsiReverseString(str); // Un-reverse string if negative.
    result := str;
    end;
end;

end.