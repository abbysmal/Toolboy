http://www.devrs.com/gb/files/sndtab.html

let frequencies =
  [
    44;
    156;
    262;
    363;
    457;
    547;
    631;
    710;
    786;
    854;
    923;
    986;
    1046;
    1102;
    1155;
    1205;
    1253;
    1297;
    1339;
    1379;
    1417;
    1452;
    1486;
    1517;
    1546;
    1575;
    1602;
    1627;
    1650;
    1673;
    1694;
    1714;
    1732;
    1750;
    1767;
    1783;
    1798;
    1812;
    1825;
    1837;
    1849;
    1860;
    1871;
    1881;
    1890;
    1899;
    1907;
    1915;
    1923;
    1930;
    1936;
    1943;
    1949;
    1954;
    1959;
    1964;
    1969;
    1974;
    1978;
    1982;
    1985;
    1988;
    1992;
    1995;
    1998;
    2001;
    2004;
    2006;
    2009;
    2011;
    2013;
    2015;
  ]

let closest n =
  List.mapi (fun idx f -> (idx, (f, abs (f - n)))) frequencies
  |> List.sort (fun (_, (_, n)) (_, (_, nn)) -> Int.compare n nn)
  |> List.hd

let () =
  let l = List.init 2023 closest in
  Printf.printf "let map_sound_to_midi = function\n";
  List.iteri
    (fun i (idx, (_, _)) -> Printf.printf "| %d -> %d\n" i (idx + 36))
    l;
  Printf.printf "| n -> 0"