-- Kamil Paluszewski 180194
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;

procedure FabrykaRowerow is
   Czesci: constant Integer := 8;
   Zestawy: constant Integer := 4;
   Odbiorcy: constant Integer := 3;
   subtype Typy_Czesci is Integer range 1 .. Czesci;
   subtype Typy_zestawu is Integer range 1 .. Zestawy;
   subtype Typy_odbiorcy is Integer range 1 .. Odbiorcy;

   Nazwa_czesci: constant array (Typy_Czesci) of String(1 .. 10)
     := ("Opona     ", "U.Napedowy", "Hamulec   ", "Niska Rama", "WysokaRama", "Swiatlo   ", "Siodelko  ", "KoloBoczne");
   Nazwa_zestawu: constant array (Typy_zestawu) of String(1 .. 12)
     := ("Rower gorski", "RowerMiejski", "R. Dzieciecy", "Tricykl     ");


   package Losowy_Zestaw is new
     Ada.Numerics.Discrete_Random(Typy_zestawu);

   package Wyjatki is
      NadmiarUtraconychCzesci : exception;
   end Wyjatki;


   -- Producent wytwarza okreslone czesci
   task type Producent is
      -- Nadawanie producentowi czesci do wytworzenia oraz czasu wykonania tej operacji
      entry Rozpocznij(Czesc: in Typy_Czesci; Czas_produkcji: in Integer);
   end Producent;

   -- Odbiorca odbiera zestaw (rower) z czesci z bufora
   task type Obiorca is
      -- Identyfikacja odbiorcy
      entry Start(Numer_odbiorcy: in Typy_odbiorcy;
                  Czas_odbioru: in Integer);
   end Obiorca;

   -- Tutaj czesci skladane sa w zestawy
   task type Przetwarzanie is
      -- Przyjmij czesc, jesli jest miejsce w magazynie
      entry Przyjmij_Czesc(Czesc: in Typy_Czesci; Numer: in Integer; Przyjeto: out Boolean);
      -- Wydaj zestaw, jezeli z czesci z magazynu da sie go zlozyc
      entry Wydaj_zestaw(Zestaw: in Typy_zestawu; Numer: out Integer; Wydano: out Boolean);
   end Przetwarzanie;

   P: array ( 1 .. Czesci ) of Producent;
   O: array ( 1 .. Odbiorcy ) of Obiorca;
   B: Przetwarzanie;

   task body Producent is
      subtype Mozliwy_Czas_Produkcji is Integer range 6 .. 12;
      package Losowa_Produkcja is new
        Ada.Numerics.Discrete_Random(Mozliwy_Czas_Produkcji);
      G: Losowa_Produkcja.Generator;	--  generator liczb losowych
      Numer_typu_czesci: Integer;
      Numer_czesci: Integer;
      Produkcja: Integer;
      Przyjeto: Boolean;
   begin
      accept Rozpocznij(Czesc: in Typy_Czesci; Czas_produkcji: in Integer) do
         Losowa_Produkcja.Reset(G);	-- ustaw generator czasu produkcji
         Numer_czesci := 1;
         Numer_typu_czesci := Czesc;
         Produkcja := Czas_produkcji;
         Przyjeto := false;
      end Rozpocznij;
      Put_Line("Rozpoczeto produkcje " & Nazwa_czesci(Numer_typu_czesci));
      loop
         delay Duration(Losowa_Produkcja.Random(G)); --  symuluj produkcje
         Put_Line("Wyprodukowano " & Nazwa_czesci(Numer_typu_czesci)
                  & " o numerze "  & Integer'Image(Numer_czesci));
         -- przyjmowanie do magazynu
         loop
            B.Przyjmij_Czesc(Numer_typu_czesci, Numer_czesci, Przyjeto);
            if Przyjeto then
               Numer_czesci := Numer_czesci + 1; -- jezeli przyjeto, zwieksz nr seryjny czesci o 1
               exit;
            else
               Put_Line("Magazyn jest zajety, ponowna proba za 4 sekundy"); -- jezeli magazyn jest nieosiagalny (np. nie przyjmuje z powodu przepelnienia), to czesc oczekuje
               delay Duration(4.0);
            end if;
         end loop;
      end loop;
   end Producent;

   task body Obiorca is
      subtype Mozliwy_Czas_Odbioru is Integer range 4 .. 7;
      package Losowy_odbior is new
        Ada.Numerics.Discrete_Random(Mozliwy_Czas_Odbioru);
      G: Losowy_odbior.Generator;	--  generator liczb losowych (czas odbioru zestawu)
      G2: Losowy_Zestaw.Generator;	--  generator liczb losowych (rodzaj zestawu)
      Nr_odbiorcy: Typy_odbiorcy;
      Numer_zestawu: Integer;
      Odbior: Integer;
      Typ_zestawu: Integer;
      Wydano: Boolean;

      Nazwa_odbiorcy: constant array (1 .. Odbiorcy)
        of String(1 .. 16)
        := ("SklepInternetowy", "Sklep Rowerowy  ", "Sklep na bazarze");
   begin
      accept Start(Numer_odbiorcy: in Typy_odbiorcy;
                   Czas_odbioru: in Integer) do
         Losowy_odbior.Reset(G);	--  ustaw generator czasu odbioru zestawu
         Losowy_Zestaw.Reset(G2);	--  tez
         Nr_odbiorcy := Numer_odbiorcy;
         Odbior := Czas_odbioru;
         Wydano := false;
      end Start;
      Put_Line("Konsumentem jest " & Nazwa_odbiorcy(Nr_odbiorcy));
      delay Duration(Losowy_odbior.Random(G)); --  symulacja odbioru zestawu(roweru)

      loop
         Typ_zestawu := Losowy_Zestaw.Random(G2);
         Put_Line(Nazwa_odbiorcy(Nr_odbiorcy) & ": zamowil " & Nazwa_zestawu(Typ_zestawu));
         -- skladanie zamowienia
         loop
            select
               B.Wydaj_zestaw(Typ_zestawu, Numer_zestawu, Wydano);
               if Wydano then
                  Put_Line(Nazwa_odbiorcy(Nr_odbiorcy) & ": ODEBRAL ROWER " &  Nazwa_zestawu(Typ_zestawu) & " O NUMERZE SERYJNYM " & Integer'Image(Numer_zestawu)); -- wydano zamowienie
                  delay Duration(Odbior);
                  exit;
               else
                  Put_Line("Chwilowy brak czesci do realizacji zestawu " & Nazwa_zestawu(Typ_zestawu)); -- brak czesci dla zestawu
                  delay Duration(8.0);
               end if;
            else
               Put_Line("Zamowiony przez " & Nazwa_odbiorcy(Nr_odbiorcy) & " zestaw " & Nazwa_Zestawu(Typ_zestawu) & " jest w kolejce"); -- zamowienie przyjete, wstawiono do kolejki
               delay Duration(10.0);
            end select;
         end loop;
      end loop;
   end Obiorca;

   task body Przetwarzanie is
      Pojemnosc_Magazynu: constant Integer := 35;
      type Typ_magazynu is array (Typy_Czesci) of Integer;
      Magazyn: Typ_magazynu
        := (0, 0, 0, 0, 0,0,0,0);
      Sklad_zestawu: array(Typy_zestawu, Typy_Czesci) of Integer -- z jakich i ilu czesci skladaja sie rowery
        := ((2,1,2,0,1,2,1,0),
            (2,1,2,1,0,1,1,0),
            (2,1,1,1,0,3,1,2),
            (3,1,1,1,0,2,1,0));
      Max_sklad_zestawu: array(Typy_Czesci) of Integer;
      Numer_seryjny_zestawu: array(Typy_zestawu) of Integer
        := (1, 1, 1, 1);
      W_Magazynie: Integer := 0;

      NieprzyjeteCzesciZRzedu : Integer := 0;

      procedure Ustaw_zmienne is
      begin
         for W in Typy_Czesci loop
            Max_sklad_zestawu(W) := 0;
            for Z in Typy_zestawu loop
               if Sklad_zestawu(Z, W) > Max_sklad_zestawu(W) then
                  Max_sklad_zestawu(W) := Sklad_zestawu(Z, W);
               end if;
            end loop;
         end loop;
      end Ustaw_zmienne;

      function Czy_Przyjac(Czesc: Typy_Czesci) return Boolean is
         Wolne_miejsce: Integer;		--  wolne miejsce w magazynie
         -- ile jakiego typu czesci potrzeba
         Brakujaca_liczba_czesci: array(Typy_Czesci) of Integer;
         -- ile miejsca w magazynie brakuje
         Brakujace_miejsce: Integer;
         Mozna_Zlozyc: Boolean;			--  czy da sie zlozyc zestaw
      begin
         if W_Magazynie >= Pojemnosc_Magazynu then
            return False;
         end if;
         -- obliczanie wolnego miejsca w magazynie
         Wolne_miejsce := Pojemnosc_Magazynu - W_Magazynie;
         Mozna_Zlozyc := True;
         for W in Typy_Czesci loop
            if Magazyn(W) < Max_sklad_zestawu(W) then
               Mozna_Zlozyc := False;
            end if;
         end loop;
         if Mozna_Zlozyc then
            return True;		--  da sie zlozyc zestaw z czesci w magazynie
         end if;
         if Integer'Max(0, Max_sklad_zestawu(Czesc) - Magazyn(Czesc)) > 0 then
            -- jezeli wlasnie tej czesci brakuje do produkcji, przyjmij ja
            return True;
         end if;
         Brakujace_miejsce := 1;			--  wstaw czesc
         for W in Typy_Czesci loop
            Brakujaca_liczba_czesci(W) := Integer'Max(0, Max_sklad_zestawu(W) - Magazyn(W));
            Brakujace_miejsce := Brakujace_miejsce + Brakujaca_liczba_czesci(W);
         end loop;
         if Wolne_miejsce >= Brakujace_miejsce then
            -- wystarcza miejsca w magazynie
            return True;
         else
            Put_Line(Integer'Image(Wolne_miejsce));
            -- brak miejsca w magazynie
            return False;
         end if;
      end Czy_Przyjac;

      function Czy_Wydac(Assembly: Typy_zestawu) return Boolean is
      begin
         for W in Typy_Czesci loop
            if Magazyn(W) < Sklad_zestawu(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Czy_Wydac;

      procedure Wypisz_Magazyn is
      begin
         for W in Typy_Czesci loop
            Put_Line("Magazyn zawiera: " & Integer'Image(Magazyn(W)) & " " & Nazwa_czesci(W)); -- wypisywanie skladu magazynu
         end loop;
      end Wypisz_Magazyn;

      procedure nadmiernaUtrataCzesci is
      begin
         if NieprzyjeteCzesciZRzedu >= 6 then
            raise Wyjatki.NadmiarUtraconychCzesci with "Nie udalo sie wstawic czesci do magazynu zbyt wiele razy. Magazyn zostanie wyczyszczony do zera";
         end if;
      end nadmiernaUtrataCzesci;

   begin
      Put_Line("Bufor zaczal prace");
      Ustaw_zmienne;
      loop
         select -- SPOTKANIE SELEKTYWNE - wybor miedzy przyjeciem czesci a wydaniem roweru odbiorcy
            accept Przyjmij_Czesc(Czesc: in Typy_Czesci; Numer: in Integer; Przyjeto: out Boolean) do

               begin
                  -- decydowanie, czy czesc moze zostac przyjeta do magazynu
                  if Czy_Przyjac(Czesc) then
                     Put_Line("Przyjeto do magazynu czesc " & Nazwa_czesci(Czesc) & " o numerze " &
                                Integer'Image(Numer));
                     Magazyn(Czesc) := Magazyn(Czesc) + 1;
                     W_Magazynie := W_Magazynie + 1;
                     Przyjeto := true;
                     NieprzyjeteCzesciZRzedu := 0;

                  else
                     Put_Line("Nie przyjeto do magazynu czesc " & Nazwa_czesci(Czesc) & " CZEKAM NA ZWOLNIENIE MIEJSCA W MAGAZYNIE");
                     Przyjeto := false; -- magazyn nie ma miejsca, np. przez rezerwacje miejsca dla potrzebniejszej czesci
                     NieprzyjeteCzesciZRzedu := NieprzyjeteCzesciZRzedu + 1;

                  end if;

               end;
            end Przyjmij_Czesc;
         or
            accept Wydaj_zestaw(Zestaw: in Typy_zestawu; Numer: out Integer; Wydano: out Boolean) do

               if Czy_Wydac(Zestaw) then
                  Put_Line("Wydano zestaw " & Nazwa_zestawu(Zestaw) & " o numerze " &
                             Integer'Image(Numer_seryjny_zestawu(Zestaw)));
                  for W in Typy_Czesci loop
                     Magazyn(W) := Magazyn(W) - Sklad_zestawu(Zestaw, W);
                     W_Magazynie := W_Magazynie - Sklad_zestawu(Zestaw, W);
                  end loop; -- zestaw wydany, nadawanie nr seryjnego zestawu
                  Numer := Numer_seryjny_zestawu(Zestaw);
                  Numer_seryjny_zestawu(Zestaw) := Numer_seryjny_zestawu(Zestaw) + 1;
                  Wydano := true;
               else
                  Wydano := false;
               end if;
            end Wydaj_zestaw;
         end select;
         Wypisz_Magazyn;

         begin
            nadmiernaUtrataCzesci;
         exception
            when wyjatek : Wyjatki.NadmiarUtraconychCzesci =>
               New_Line;
               put_line(Ada.Exceptions.Exception_Message(wyjatek));
               for W in Typy_Czesci loop
                  Magazyn(W) := 0;
               end loop;
               NieprzyjeteCzesciZRzedu := 0;
               W_Magazynie := 0;
               Wypisz_Magazyn;

         end;

      end loop;

   end Przetwarzanie;

begin
   for I in 1 .. Czesci loop
      P(I).Rozpocznij(I, 10);
   end loop;
   for J in 1 .. Odbiorcy loop
      O(J).Start(J,12);
   end loop;
end FabrykaRowerow;

