# imperative programming vefst fyrir mörgum það er basically bara c pointer hell á meðan functional programming er f#
* call by value er að henda inn kópíu af parametrum inn í föll og munu þá ekki breytast globally
* call by reference er verið að senda inn minnisaddressur í fallið og þá munu þau breytast fyrir utan fallið
* call by value result er gamalt shit sem var í gömlu málum eins og fortran og er víst helvíti það er eins og   call by value nema það yfirskrifar parametrana á returninu


# í static scope geta breytr verið 3 möguleikar:
* skilgreind í fallinu sem compilerinn er í
* parameter inn í fallið
* global breyta

# í flestum tungumálum í dag eru breytur static static scope og óháðar stakknum

# dynamic scope er ekki algengt í nútíma tungumálum, þá leitar hann fyrst í fallinu sem hann er í
# svo fer hann niður í fallið sem kallar í það osfrv osfrv niður stakkinn þangað til hann endar á global breytunni
í dynamic scope snýst allt um að hafa main fallið neðst í röðinni 
Hann fer niður stakkinn og leitar í næsta og hendir svo því sem hann er búinn að nota af stakknum

# tips með að lesa dynamic og static úr f# er að taka orðið "in" í burtu
let x = 17 in
  let f y = x + y in
    let y = 3 in
      let x = 42 in
        f (y * 2)
verður að þessu
x = 17
f y = x + y
  y = 3
  x = 42
f (y * 2)

# perl styður bæði dynamic og static
Við getum farið inn á perl playgroundið ef við erum spurð út í muninn á dynamic og static
sett  þar inn kóðann í perl og séð hvernig þetta hagar sér

# í firstfun í fyrirlestri 9 er hann með aðferð til að sýna muninn á dynamic og static scope
þar getur maður commentað út og inn



# við notum expressionary grammar en ekki context free grammar

# í disambiguate labbar maður frá vinstri til hægri
allir stærðfræði operatorar eru left associative

* er meira pwerful en -

x * y - z

* er left associatives
(x * y) - z

* er right associative
x * (y - z)

left associative er þannig að a + b + c verður (a+b)+c. þú lest frá vinstri til hægri og setur sviga utan um accordingly. right associative er svo öfugt
ef + væri right associative verður a+b+c -> a+(b+c)

það þarf ekki að bera saman operatora sem er ´buið að encapsuleita
þannig ef ég er með 
x // (Y // z) // 
þá ber ég saman operator lengst til vinstri og lengst til hægri


let x = 17 in
  let f y = x + y in
    let y = 3 in
      let x = 42 in
        f (y * 2)


# closure er stóra F-ið F ("g", "y", Add (Var "y", Var "z"), ["z", 9])
í því er hægt að setja function nafnið í variable



# svar  29. c er nei afþví það er ekki recursion í þessu dæmi


# call by value er parameter passing mode

# þú getur ekki skilgreint function inni í function í c


## googla eitthvað heiti og computer science


# ef þú ert að skilgreina eitthvað sem er í sama minni þá verður það að vera sama týpa

## tvær leiðir af lambda functions
* turing complete
* lambda function progress

# (λx. x) er eins og let f x = x  í F# útfærslu

## lambda abstraction er left associative, 
## en application er right associative






