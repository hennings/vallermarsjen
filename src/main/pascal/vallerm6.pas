{ 8192,0,0}
{ H & H -Mykvare
}
PROGRAM VallerMarsj;

USES dos,crt,printer,diverse;

CONST
        NetMelding = FALSE;
        logPathFil = 'LOGGEN.VMA';
        listeantall=11;
        splitline=16;
        spc='                                                                   ';
        buf = 64;
        maxrecords = 250;

TYPE
        NavneType=STRING[40];
        sex=(Gu,Je);
{        path_name=ARRAY [1..80] OF CHAR;}
	resrecord = RECORD
		name   	:NavneType;
		klasse	:NavneType;
		skole	:NavneType;
		starttid,innkomst,sluttid:LONGINT;
                kjonn   :SEX;
	END;


VAR 	l,i,num,st,en,onr,ten,tst,nen,nst,j,f,nsafe,qsafe,respage,
        d,e,select,t,efternavn
			:INTEGER;
	a,b,c,tsafe,jj,ll
                        :LONGINT;
	resname,fork,dummy,da,db,dc,pr,valg
			:STRING;
        statlst         :TEXT;
{        lst		:TEXT; }
	handle		:FILE OF resrecord;
	results		:ARRAY [0..maxrecords] OF resrecord;
	tidbuffer	:ARRAY [1..buf] OF LONGINT;
	nrbuffer	:ARRAY [1..buf] OF INTEGER;
	sort		:ARRAY [1..maxrecords] OF INTEGER;
        exit            :BOOLEAN;
        ff              :CHAR;


PROCEDURE WrL(a:STRING;b:INTEGER);FORWARD;

PROCEDURE ClrReminder;
BEGIN
   Write(' ':(79-whereX));
{   REPEAT
     Write(' ');
   UNTIL WhereX>78;  }
WriteLn;
END;


PROCEDURE CursOn;
BEGIN
   asm
     MOV AH,01
     MOV CH,$1E      { Start 0-$1F - $20 no cursor }
     MOV CL,$1F      { Endl  0-$1f }
     INT 10H
   END;
END;
PROCEDURE PBottom;
var Hour, Minute, Second, Sec100: Word;
    Year, Month, Day, DayOfWeek: Word  ;
BEGIN
   WriteLn(lst);
   Gettime(Hour, Minute, Second, Sec100);
   GetDate(Year, Month, Day, DayOfWeek);
   WriteLn(lst,'---- ',hour,':',minute,'.',second,'  ',day,'/',month,'-',year,' ---- Vallermarsjen      HH-Tid 1993');
   WriteLn (lst);
   WriteLn (lst);
   WriteLn (lst);
   {
   WriteLn(lst,chr(12));
   }
END;

PROCEDURE FindName(name:STRING;leng:INTEGER;VAR stnr:INTEGER);
VAR nr,l,antt:INTEGER;
    tt:resrecord;
    a,b:STRING;
BEGIN
   Write('.');
   l:=0;antt:=0;stnr:=0;
   REPEAT
     l:=l+1;
     tt:=results[l];
     a:=UCASE(COPY(tt.name,1,leng));
     b:=UCASE(COPY(name,1,leng));
     IF (a=b) THEN BEGIN
        antt:=antt+1;
        WriteLn;
        Write(l:4,' ');
        WrL(tt.name,30);
        Write(' Klasse: ');
        WrL(tt.klasse,10);
        IF tt.kjonn=Gu THEN Write(' Gutt ');
        IF tt.kjonn=Je THEN Write(' Jente');
        stnr:=l;
     END;
   UNTIL l=maxrecords;
   IF (antt=0) AND (leng>2) THEN BEGIN
       FindName(name,leng-1,stnr);
   END;
   IF (antt=0) THEN stnr:=0;
   IF antt>1 THEN BEGIN
       WriteLn;
       Write('Hvilket navn velger du ? ');
       ReadLn(name);
       IF name<>'' THEN FindName(name,length(name),stnr) ELSE stnr:=0;
   END;
   WriteLn;
END;

PROCEDURE CursOff;
BEGIN
   asm
     MOV AH,01
     MOV CH,$20      { Start 0-$1F - $20 no cursor }
     MOV CL,$20      { Endl  0-$1f }
     INT 10H
   END;
END;

PROCEDURE ClrBottom;
BEGIN
   REPEAT
     ClrReminder;
   UNTIL Wherey=23;
END;

PROCEDURE NyeLister;FORWARD;

PROCEDURE Gettext(forklaring:STRING;VAR verdi:NavneType);
VAR inn:STRING;
BEGIN
  CursOn;
  Write(forklaring);
  verdi:=EditString(verdi, NetMelding);
(*  ,verdi,CHR(13),forklaring);
  ReadLn(inn);
  IF Length(inn)>0 THEN verdi:=inn;
  GotoXY(whereX,whereY-1);
  Write(forklaring,verdi);
  ClrReminder;   *)
  CursOff;
END; {proc gettext}

PROCEDURE Gettext2(forklaring:STRING;VAR verdi:STRING);
VAR inn:STRING;
BEGIN
  CursOn;
  Write(forklaring,verdi,CHR(13),forklaring);
  ReadLn(inn);
  IF Length(inn)>0 THEN verdi:=inn;
  GotoXY(whereX,whereY-1);
  Write(forklaring,verdi);
  ClrReminder;
  CursOff;
END; {proc gettext}


FUNCTION Clock:LONGINT;
VAR h,m,s,hund:WORD;
    t:LONGINT;
BEGIN
      GetTime(h,m,s,hund);
      t:=h;
      clock:=t*3600+m*60+s;
END;

Function File_Exist(FileName: string): Boolean;
Var
  f: file;
Begin
 {$I-}
 Assign(f, FileName);
 Reset(f);
 Close(f);
 {$I+}
 File_Exist := (IOResult = 0) and (FileName <> '');
End;  { FileExists }

PROCEDURE GetFileName_ReadFile;
VAR i:INTEGER;
BEGIN
   resname:='VALLMA94.DAT';
   Gettext2('Navn og tilgangsbane for resultatfil:',resname);
   IF FileExists(resname)=FALSE THEN BEGIN
      WriteLn('Creating file...',resname);
      Assign(handle,resname);
      ReWrite(handle);
      FOR i:=1 TO maxrecords DO BEGIN
         Write(i:4,'/',maxrecords,chr(13));
         Write(handle,results[i]);
      END;
      CLOSE(handle);
      WriteLn;
   END;
   ASSIGN(handle,resname);
   RESET(handle);
   WriteLn(filesize(handle));
   l:=0;
   WHILE NOT EOF(handle) DO BEGIN
      l:=l+1;
      READ(handle,results[l]);
      results[l].klasse:=LCASE(results[l].klasse);
      If l>100 THEN
        Write ;
   END;
END;

PROCEDURE WrL(a:STRING;b:INTEGER);
BEGIN
   Write(COPY(CONCAT(a,spc),1,b));
END;

PROCEDURE PrL(a:STRING;b:INTEGER);
BEGIN
   Write(lst,COPY(CONCAT(a,spc),1,b));
END;


PROCEDURE ReadV(da:STRING;VAR a:longInt);
var Code:Integer;
begin
 Val(da, a, Code);
 if code <> 0 then Writeln('Error at position: ', Code);
end;

PROCEDURE WriteV(VAR da:STRING;a:LONGINT);
BEGIN
 Str(a,da);
END;



PROCEDURE WrHMS(c:LONGINT);
VAR a,b:LONGINT;
    da,db,dc,ooo:STRING;
BEGIN
   IF c=0 THEN Write('--------');
   IF (c>0) AND (c<1000000) THEN BEGIN
      ooo:='0000000';
      a:=c DIV 3600;
      b:=(c-a*3600) DIV 60;
      c:=c MOD 60;

      WriteV(da,a);da:=CONCAT(ooo,da);da:=COPY(da,Length(da)-1,2);
      WriteV(db,b);db:=CONCAT(ooo,db);db:=COPY(db,Length(db)-1,2);
      WriteV(dc,c);dc:=CONCAT(ooo,dc);dc:=COPY(dc,Length(dc)-1,2);
      Write(da,':',db,'.',dc);
   END;
   IF (c=1000001) THEN Write(' Disket ');
   IF (c=1000000) THEN Write(' Brutt  ');
   IF c<0 THEN Write(' Sick!  ');
END;

PROCEDURE PrHMS(c:LONGINT);
VAR a,b:LONGINT;
    da,db,dc,ooo:STRING;
BEGIN
   IF c=0 THEN Write(lst,'        ');
   IF (c>0) AND (c<1000000) THEN BEGIN
      ooo:='0000000';
      a:=c DIV 3600;
      b:=(c-a*3600) DIV 60;
      c:=c MOD 60;

      WriteV(da,a);da:=CONCAT(ooo,da);da:=COPY(da,Length(da)-1,2);
      WriteV(db,b);db:=CONCAT(ooo,db);db:=COPY(db,Length(db)-1,2);
      WriteV(dc,c);dc:=CONCAT(ooo,dc);dc:=COPY(dc,Length(dc)-1,2);
      Write(lst,da,':',db,'.',dc);
   END;
   IF c=1000001 THEN Write(lst,' Disket ');
   IF c=1000000 THEN Write(lst,' Brutt  ');
   IF c<0 THEN Write(' Sick!  ');
END;

PROCEDURE Loparstatus(j:INTEGER);
BEGIN
   GotoXY(1,9);Write(j:4,'     '); WrL(results[j].name,21);
   IF (results[j].kjonn=Gu) THEN Write(' G') ELSE Write(' J');
   GotoXY(10,10);WrL(results[j].skole,22);
   GotoXY(10,11);WrL(results[j].klasse,22);
   GotoXY(1,12);Write('Innkomst ');
   WrHMS(tidbuffer[tst]);
   GotoXY(1,13);Write('Sluttid  ');WrHMS(results[j].sluttid);WriteLn;
END;



PROCEDURE ScStartListe;
VAR i:INTEGER;
BEGIN
   st:=onr-8;
   en:=onr+8;
   IF st<1 THEN BEGIN
      en:=en-(st-1);
      st:=1;
   END;
   IF en>maxrecords THEN BEGIN
      st:=st-(en-maxrecords);
      en:=maxrecords;
   END;
   FOR i:=st TO en DO BEGIN
      Write(i:5,' ');
      WrL(results[i].name,20);
      WrL(results[i].klasse,6);
      WrL(results[i].skole,14);
      WrHMS(results[i].starttid);
      Write('  ');
      WrHMS(results[i].sluttid);
      WriteLn;
   END;
END;

FUNCTION GetHMS(c:LONGINT):LONGINT;
VAR a,b:LONGINT;
BEGIN
         WrHMS(c);
	 WriteLn;
         IF (c>=0) AND (c<1000000) THEN BEGIN
              WrHMS(c);
              Write(chr(13));
            END
          ELSE BEGIN
              Write('00.00.00',chr(13));
              a:=0;b:=0;c:=0;
            END;
	 CursOn;
         a:=c DIV 3600;
         b:=(c-a*3600) DIV 60;
         c:=c MOD 60;
         ReadLn(dummy);
         dummy:=CONCAT(dummy,'        ');
         da:=COPY(dummy,1,2);
    	 db:=COPY(dummy,4,2);
         dc:=COPY(dummy,7,2);
         IF (da<>'  ') THEN ReadV(da,a);
         IF (db<>'  ') THEN ReadV(db,b);
         IF (dc<>'  ') THEN ReadV(dc,c);
         GetHMS:=a*3600+b*60+c;
         CursOff;
         IF COPY(dummy,1,4)='DISK' THEN GetHMS:=1000001;
         IF COPY(dummy,1,5)='BRUTT' THEN GetHMS:=1000000;
END;

PROCEDURE PickRes;
BEGIN
   FOR i:=1 TO l DO BEGIN
      sort[i]:=0;
      IF (results[i].innkomst>0) THEN BEGIN
         t:=t+1;
         sort[t]:=i;
      END;
   END;
END;

PROCEDURE PickRes2;
BEGIN
   FOR i:=1 TO l DO BEGIN
      sort[i]:=0;
      IF (efternavn=2) AND (results[i].kjonn=Je) THEN
        IF (results[i].innkomst>0) THEN BEGIN
           t:=t+1;
           sort[t]:=i;
        END;
      IF (efternavn=3) AND (results[i].kjonn=Gu) THEN
        IF (results[i].innkomst>0) THEN BEGIN
           t:=t+1;
           sort[t]:=i;
        END;
   END;
END;

PROCEDURE Leggeinn_startliste;
VAR dummy:STRING;
    valg:INTEGER;
BEGIN
   REPEAT
      ClrScr;
      ScStartListe;
      onr:=onr+1;
      WriteV(dummy,onr);
      fork:=CONCAT('Ditt valg: (0 avslutter) ');
      Gettext2(fork,dummy);
      IF dummy<>'' THEN ReadV(dummy,jj);
      valg:=jj;
      IF valg<>0 THEN BEGIN
         onr:=valg;
         Gettext('Navn  :',(results[valg].name));
         Gettext('Klasse:',results[valg].klasse);
         IF results[valg].skole='' THEN results[valg].skole:='Valler VGS';
         Gettext('Skole :',results[valg].skole);
         IF results[valg].kjonn=Gu THEN dummy:='Gu' ELSE dummy:='Je';
         Gettext2('Kj›nn :',dummy);
         IF (COPY(dummy,1,1)='G') OR (COPY(dummy,1,1)='g') THEN results[valg].kjonn:=Gu;
         IF (COPY(dummy,1,1)='J') OR (COPY(dummy,1,1)='j') THEN results[valg].kjonn:=Je;
         c:=results[valg].starttid;
         WriteLn('Starttid:');
         results[valg].starttid:=GetHMS(results[valg].starttid);
         CursOff;
         Seek(handle,valg-1);
	 Write(handle,results[valg]);
      END; { of IF valg<>0 }
   UNTIL valg=0;
END;

PROCEDURE SortRes;
VAR i,j,f:INTEGER;
BEGIN
   t:=0;
   Results[0].starttid:=0;
   Results[0].innkomst:=0;
   Results[0].sluttid:=0;
   IF (efternavn=2) OR (efternavn=3) THEN PickRes2 ELSE PickRes;
   FOR i:=1 TO t-1 DO BEGIN
      FOR j:=i+1 TO t DO BEGIN
         IF (results[sort[j]].sluttid<results[sort[i]].sluttid) THEN BEGIN
            f:=sort[i];
            sort[i]:=sort[j];
            sort[j]:=f;
         END;
      END;
   END;
END;

PROCEDURE Efter(na:STRING;VAR na3:STRING);
BEGIN
   WHILE (POS(' ',na)>0) DO
      na:=COPY(na,POS(' ',na)+1,255);
   na3:=na;
END;

PROCEDURE SortStart;
VAR i,j,f:INTEGER;
    efter1,efter2:STRING;
BEGIN
   Results[0].starttid:=0;
   Results[0].innkomst:=0;
   Results[0].sluttid:=0;
   t:=0;
   FOR i:=1 TO l DO BEGIN
      sort[i]:=0;
      IF (results[i].name<>'') THEN BEGIN
         t:=t+1;
         sort[t]:=i;
      END;
   END;
   FOR i:=1 TO t-1 DO BEGIN
      FOR j:=i+1 TO t DO BEGIN
         IF results[sort[j]].starttid<results[sort[i]].starttid THEN BEGIN
            f:=sort[i];
            sort[i]:=sort[j];
            sort[j]:=f;
         END;
         IF Efternavn=1 THEN
           IF (results[sort[j]].starttid=results[sort[i]].starttid) THEN BEGIN
              Efter(results[sort[j]].name,efter1);
              Efter(results[sort[i]].name,efter2);
              IF efter1<efter2 THEN BEGIN
                 f:=sort[i];
                 sort[i]:=sort[j];
                 sort[j]:=f;
              END;
           END;             {* P†/AV etternavn sortering *}
         { END of Efternavn=TRUE }
      END; { of FOR j:= }
   END; { of FOR i:= }
END;

PROCEDURE Top;
VAR j:INTEGER;
BEGIN
   ClrScr;
   WriteLn('--> VallerMarsj - Tidtagning + Resultatsystem - v2.00 <--  Henning Spjelkavik');

   FOR j:=1 TO 32 DO Write('Ä');Write('Â');FOR j:=34 TO 79 DO Write('Ä');

   FOR j:=1 TO 13 DO BEGIN
     GotoXY(33,j+2);Write('³');
   END;
   GotoXy(1,splitline);
   FOR j:=1 TO 32 DO Write('Ä');Write('Á');FOR j:=34 TO 79 DO Write('Ä');
   GotoXY(1,8);
   FOR j:=1 TO 32 DO Write('Ä');Write('´');
   SortStart;
   GotoXY(43,splitline);   Write('Antall startende:',t:4);
   SortRes;
   GotoXY(3,splitline);    Write('Antall i m†l:',t:4);
   GotoXY(1,24);
   WriteLn('F2 - Legge inn  F3 - Endre tider  F9 - Lister  F10 - Exit  F - FinnNavn');
   Write('SF2 - Liten st.liste  SF3 - Res.liste  C-A Felles/CLR  C-B Folk ute  C-C Utskri');

{   WriteLn('C+R ResPag,Z PgD,Q PgU,S StL,R ResL,E Endre,s LageStartL');
   Write('C+A ClrRec,BkSpc Rett.,Enter Reg.st.nr,C+"-" ListeLengde,Spc Reg.tid'); }

END;

PROCEDURE SjekkKnappListe;
BEGIN
   IF f=0 THEN
      BEGIN { en eller annen har truffet en EXTENDED-tast }
         f:=ORD(ReadKey);
         CASE f OF
            59:f:=12;               { F1 }
            68:f:=84;               { F10 - Exit }
            73:f:=81;               { PgUP }
            81:f:=90;               { PgDWN }
         END;
    END;
         CASE f OF
            81:BEGIN   {* Q - PageUp *}
                     ll:=ll-16;
                     IF ll<0 THEN ll:=0;
               END;
            90:BEGIN   {* Z - PageDown *}
                     ll:=ll+16;
                     IF ll>maxrecords-16 THEN ll:=maxrecords-16;               END;
            18:BEGIN   {* Ctrl R - ResPage *}
                  GotoXY(1,3);
                  Write('Respage:');
                  ReadLn(dummy);
                  IF dummy<>'' THEN BEGIN
                     ReadV(dummy,ll);
                     ll:=ll-1;
                     IF ll<0 THEN ll:=0;
                     IF ll>maxrecords-16 THEN ll:=maxrecords-16;
                     GotoXY(1,3);
                     ClrReminder;
                  END;
               END;
         END; {* of CASE *}
END;

PROCEDURE StartLister;
VAR aa:LONGINT;
    i,j,test:INTEGER;
BEGIN
      SortStart;
      ll:=0;select:=2;f:=20;
      REPEAT
         GotoXY(1,4);WrHMS(clock);
         GotoXY(1,6);
         IF f>0 THEN BEGIN
            GotoXY(1,6);
            FOR i:=1 TO 16 DO BEGIN
               Write('     ');
               IF sort[i]<>0 THEN
                   Write(sort[i+ll]:4,'  ')
                ELSE
                   Write('      ');

               WrL(results[sort[ll+i]].name,24);
               IF (results[sort[ll+i]].kjonn=Gu) THEN Write(' Gu ') ELSE Write(' Je ');
               Write(' ');
               WrL(results[sort[ll+i]].klasse,5);Write(' ');
               WrL(results[sort[ll+i]].skole,20);Write(' ');
               WrHMS(results[sort[ll+i]].starttid);WriteLn;
            END;
         END;
         IF KeyPressed=TRUE THEN BEGIN
            f:=ORD(ReadKey);
            SjekkKnappListe;
         END;
      UNTIL ((f=80) OR (f=84) OR (f=82) OR (f=83) or (f=114) or (f=115));
      IF f=84 THEN exit:=TRUE;
      IF f=80 THEN BEGIN { Utskrift til skriver }
            exit:=TRUE;
            WriteLn(lst,'                        --> Startliste <--',chr(10));
            GotoXY(1,22);

            CursOn;
            WriteV(dummy,1);
            Gettext2('Fra hvilket nummer i listen ? ',dummy);
            IF dummy<>'' THEN ReadV(dummy,aa);
            WriteV(dummy,t);
            GetText2('Til hvilket ? ',dummy);
            IF dummy<>'' THEN ReadV(dummy,ll);
            CursOff;

            IF (aa>0) AND (ll<=t) THEN
            FOR i:=aa TO ll DO BEGIN
               Write(lst,'     ');
               IF sort[i]<>0 THEN
                   Write(lst,sort[i]:4,'  ')
                ELSE
                   Write(lst,'      ');
               PrL(results[sort[i]].name,30);Write(lst,' ');
               PrL(results[sort[i]].klasse,5);Write(lst,' ');
               PrL(results[sort[i]].skole,20);Write(lst,' ');
               PrHMS(results[sort[i]].starttid);WriteLn(lst);
            END;
            PBottom;
      END;
      IF (f=82) or (f=114) THEN BEGIN
         valg:='R';
         NyeLister;
      END;
      IF (f=83) or (f=115) THEN BEGIN
         valg:='S';
         NyeLister;
      END;

END;

PROCEDURE ResultatLister;
VAR aa:LONGINT;
    i,j,test:INTEGER;
BEGIN
      SortRes;
      ll:=0;select:=1;f:=20;
      REPEAT
         GotoXY(1,4);WrHMS(clock);
         GotoXY(1,6);
         IF f>0 THEN BEGIN

            FOR i:=1 TO 16 DO BEGIN
               test:=i+ll;
               REPEAT
                  IF test>1 THEN
                     IF (results[sort[i+ll]].sluttid=results[sort[test-1]].sluttid) THEN
                         test:=test-1;
               UNTIL ((test=1) OR (results[sort[ll+i]].sluttid<>results[sort[test-1]].sluttid));

               IF (results[sort[i+ll]].sluttid>0) AND (results[sort[i+ll]].sluttid<1000000) THEN
                   Write(test:4,' ')
                 ELSE
                   Write('     ');

               IF (results[sort[i+ll]].sluttid>0) THEN
                   Write(sort[i+ll]:4,'  ')
                ELSE
                   Write('      ');

               WrL(results[sort[ll+i]].name,25);Write(' ');
               WrL(results[sort[ll+i]].klasse,5);Write(' ');
               WrL(results[sort[ll+i]].skole,13);Write(' ');
               WrHMS(results[sort[ll+i]].sluttid);
               Write('   ');WrHMS(results[sort[ll+i]].sluttid-results[sort[1]].sluttid);
               WriteLn;
            END;
         END;
         f:=0;
         IF KeyPressed=TRUE THEN BEGIN
            f:=ORD(ReadKey);
            SjekkKnappListe;
         END;
      UNTIL ((f=80) OR (f=84) OR (f=82) OR (f=83) or (f=114) or (f=115));
      IF f=84 THEN exit:=TRUE;
      IF f=80 THEN BEGIN
            exit:=TRUE;
            WriteLn(lst,'                        --> Resultatliste <--',chr(10));

            CursOn;
            WriteV(dummy,1);
            GotoXY(1,22);
            Gettext2('Fra hvilket nummer i listen ? ',dummy);
            IF dummy<>'' THEN ReadV(dummy,aa);
            WriteV(dummy,t);
            GetText2('Til hvilket ? ',dummy);
            IF dummy<>'' THEN ReadV(dummy,ll);
            CursOff;

            IF (aa>0) AND (ll<=t) THEN

            FOR i:=aa TO ll DO BEGIN
               test:=i;
               REPEAT
                  IF i>1 THEN
                     IF (results[sort[test]].sluttid=results[sort[test-1]].sluttid) THEN
                         test:=test-1;
               UNTIL ((test=1) OR (results[sort[test]].sluttid<>results[sort[test-1]].sluttid));
               IF (results[sort[i]].sluttid>0) AND (results[sort[i]].sluttid<1000000) THEN
                   Write(lst,test:4,' ')
                 ELSE
                   Write(lst,'     ');

               IF (results[sort[i]].sluttid>0) THEN
                   Write(lst,sort[i]:4,'  ')
                ELSE
                   Write(lst,'      ');

               PrL(results[sort[i]].name,25);Write(lst,' ');
               PrL(results[sort[i]].klasse,5);Write(lst,' ');
               PrL(results[sort[i]].skole,13);Write(lst,' ');
               PrHMS(results[sort[i]].sluttid);
               Write(lst,'   ');
               IF results[sort[i]].sluttid<1000000 THEN PrHMS(results[sort[i]].sluttid-results[sort[1]].sluttid);
               WriteLn(lst);
            END; { End of FOR , End of IF }
            PBottom;
      END;
      IF (f=82) or (f=114) THEN BEGIN
         valg:='R';
         NyeLister;
      END;
      IF (f=83) or (f=115) THEN BEGIN
         valg:='S';
         NyeLister;
      END;

END;

PROCEDURE NyeLister;
BEGIN
   IF exit=FALSE THEN BEGIN
      IF (valg='R') OR (valg='r') THEN BEGIN
         ResultatLister;
      END; { IF Valg=R }
      IF (valg='S') OR (valg='s') THEN BEGIN
         StartLister;
      END; { IF Valg=S }
   END;
END;

PROCEDURE Lister;
BEGIN
   exit:=FALSE;
   ClrScr;
   Write('Du kan velge mellom: (R)esultatliste, (S)tartliste (T)ilbake  ');
   CursOn;
   ReadLn(valg);
   CursOff;
   ClrScr;
   Write('Valg: (R)esultatliste  (S)tartliste  (P)rinter  F10 eller (T)ilbake');
   NyeLister;
END;

PROCEDURE VelgSelv;
VAR test,i:INTEGER;
BEGIN
      CASE select OF
         1:BEGIN
            SortRes;
            FOR i:=1 TO listeantall DO BEGIN
               GotoXy(35,i+3);
               test:=i+respage;
               REPEAT
                  IF test>1 THEN
                     IF (results[sort[i+respage]].sluttid=results[sort[test-1]].sluttid) THEN
                          test:=test-1;
               UNTIL ((test=1) OR (results[sort[respage+i]].sluttid<>results[sort[test-1]].sluttid));

               IF (results[sort[respage+i]].sluttid>0) AND (results[sort[respage+i]].sluttid<1000000) THEN
                      Write(test:4,' ') ELSE Write('     ');
               IF (results[sort[respage+i]].sluttid>0) THEN
                      Write(sort[i+respage]:4,'  ') ELSE Write('      ');
               WrL(results[sort[respage+i]].name,20);Write(' ');
               WrHMS(results[sort[respage+i]].sluttid);
            END;
            GotoXY(3,splitline);
            Write('Antall i m†l:',t:4);
         END;
         2:BEGIN
            SortStart;
            FOR i:=1 TO listeantall DO BEGIN
               GotoXy(35,i+3);
               Write('St.nr');
               IF sort[respage+i]<>0 THEN
                   Write(sort[i+respage]:4)
                ELSE
                   Write('    ');
               IF results[sort[respage+i]].sluttid>0 THEN Write('û ') ELSE Write('  ');
               WrL(results[sort[respage+i]].name,20);Write(' ');
               WrHMS(results[sort[respage+i]].starttid);
            END;
            GotoXY(43,splitline);
            Write('Antall startende:',t:4);
         END;
      END; { of CASE }
END;

PROCEDURE SjekkKnapp;       { Merke 1 : !"# }
VAR j,i,j2:INTEGER;
    starten:LONGINT;
    stnr,l3:INTEGER;
    cl,valg:STRING;
BEGIN
   IF f=0 THEN
      BEGIN { en eller annen har truffet en EXTENDED-tast }
         f:=ORD(ReadKey);
         CASE f OF
            59:f:=12;               { F1 }
            60:f:=115;              { F2 - Starliste, legge inn }
            61:f:=69;               { F3 - Endre tider m.m }
            67:f:=76;               { F9 - Lister }
            68:f:=27;               { F10 - Exit }
            73:f:=81;               { PgUP }
            81:f:=90;               { PgDWN }
            85:f:=83;               { S-F2 - Str.liste vindu }
            86:f:=82;               { S-F3 - Res.liste vindu }
         END;
    END;

         CASE f OF
            102:BEGIN  { f - FindName }
                ClrScr;
                stnr:=0;
                CursOn;
                Write('Hvilket navn s›ker du ? ');
                ReadLn(valg);
                CursOff;
                FindName(valg,length(valg),stnr);
                Top;
                GotoXY(1,9);
                GotoXY(1,9);
                Write(stnr:4,'     '); WrL(results[stnr].name,21);
               END;
(*    	    27:Halt;   {* Escape *}  *)
             1:BEGIN {* CTRL A - Clear records *}
                  GotoXY(1,splitline+1);
                  Write('Slette ? ');
                  CursOn;
                  WriteLn('NavnStartInnkSlutt -> Intet: xxxx Alt: 1111 Vanlig: xx11 ');
                  WriteLn('                      Sette fellesstarttid: x2xx');
                  ReadLn(cl);
                  IF Copy(cl,2,1)='2' THEN BEGIN
                     WriteLn('Hvilken starttid skal alle f†?');
                     starten:=results[1].starttid;
                     starten:=GetHMS(starten);
                  END;
                  CursOff;
                  FOR i:=1 TO maxrecords DO BEGIN
                     Write(i:5,' / ',maxrecords,chr(13));
                     IF Copy(cl,1,1)='1' THEN results[i].name:='';
                     IF Copy(cl,2,1)='1' THEN results[i].starttid:=0;
                     IF (Copy(cl,2,1)='2') AND (results[i].name<>'') THEN results[i].starttid:=starten;
                     IF Copy(cl,3,1)='1' THEN results[i].innkomst:=0;
                     IF Copy(cl,4,1)='1' THEN results[i].sluttid:=0;
                     Seek(handle,i-1);
                     Write(handle,results[i]);
                  END;
                  GotoXY(1,splitline+1);
                  ClrReminder;
                  ClrReminder;
               END;
             2:BEGIN   { C-B Folk som ikke er i m†l! }
                  ClrScr;
                  WriteLn('Folk som ikke er i m†l:',chr(10));
                  l3:=1;
                  For i:=1 TO maxrecords DO BEGIN
                     IF (results[i].name<>'') AND (results[i].innkomst=0) THEN BEGIN
                        Write(i:4,' ');
                        WrL(results[i].name,30);
                        WrL(results[i].klasse,10);
                        WrL(results[i].skole,10);
                        WriteLn;
                        l3:=l3+1;
                     END;
                     IF l3 MOD 20=0 THEN BEGIN
                        Write('Trykk en tast',chr(13));
                        ff:=ReadKey;
                        Write('             ',chr(13));
                     END;
                  END;
                  WriteLn;
                  WriteLn('Antall : ',l3-1);
                  ff:=ReadKey;
                  Top;
               END;
             3:BEGIN
                 GotoXY(1,splitline+1);
                 Write(efternavn:2,' Utskrift: 1=P† 2=Jenteliste 3=Gutteliste 4=Skriv til fil  :');
                 ReadLn(efternavn);
                 ClrBottom;
                 If efternavn=4 THEN BEGIN
                    Assign(lst,'printout.txt');
                    IF NOT Fileexists('printout.txt') THEN Rewrite(lst);
                    Append(lst);
                 END;
               END;
            76:BEGIN            { F9 - Lister }
                  Lister;
                  Top;
               END;
             8:BEGIN {* Backspace = Rettelse *}
                  GotoXy(1,splitline+1);
                  Write(qsafe:5,' ',results[qsafe].name,'  ');
                  WrHMS(results[qsafe].sluttid);Write(' ');
                  WrHMS(results[qsafe].innkomst);WriteLn;
                  Write('Riktig startnummer :');
                  CursOn;
                  ReadLn(dummy);
                  CursOff;
                  IF dummy<>'' THEN BEGIN
                     ReadV(dummy,jj);
                     j:=jj;
                     IF qsafe>0 THEN BEGIN
                       results[qsafe].innkomst:=tsafe;
                       results[qsafe].sluttid:=tsafe-results[qsafe].starttid;
                       Seek(handle,qsafe-1);
              	       Write(handle,results[qsafe]);
                     END;
                     tsafe:=results[j].innkomst;
                     qsafe:=j;
   	   	     nrbuffer[nsafe]:=j;
                     results[j].innkomst:=tidbuffer[nsafe];
                     results[j].sluttid:=results[j].innkomst-results[j].starttid;

{                     GotoXY(1,3);
                     Write(j:4,' ',results[j].name);
                     ClrReminder;
                     WrHMS(tidbuffer[nsafe]);Write('   ');
                     WrHMS(results[j].sluttid);WriteLn; }
                     LoparStatus(j);

                     GotoXy(1,splitline+1);
                     ClrReminder;
                     ClrReminder;
                     IF j>0 THEN BEGIN
                        Seek(handle,j-1);
                        Write(handle,results[j]);
                        VelgSelv;
                     END;
                  END;
               END;
            13:BEGIN   {* Enter - Registrere startnummer *}
                  GotoXY(1,splitline+1);
                  Write('Startnummer :');
                  CursOn;
                  ReadLn(dummy);
                  CursOff;
                  GotoXy(1,splitline+1);
{                  Write(chr(27),'l');   }
                  IF dummy<>'' THEN BEGIN
                     ReadV(dummy,jj);
                     j:=jj;
                     IF j>0 THEN BEGIN
                        nrbuffer[nen]:=j;
                        nen:=nen+1;
                        IF nen>buf THEN nen:=1;
                        END
                      ELSE BEGIN
                        Gotoxy(1,9);
                        WrL('0000 ',31);WriteLn;
                        WrL('',31);WriteLn;
                        WrL('',31);WriteLn;
                        WrL('',31);WriteLn;
                        WrL('',31);WriteLn;
                        nen:=nst;
                        ten:=tst;
                     END;
                  END;
               END;
            32:BEGIN   {* Registrere tid *}
                  tidbuffer[ten]:=clock;
                  ten:=ten+1;
                  IF ten>buf THEN ten:=1;
               END;
            115:BEGIN   {* s - Legge inn startliste   >F2 *}
                  Leggeinn_startliste;
                  Top;
               END;
	    69:BEGIN    {* E - Endre tider  >F3 *}
                  CursOn;
                  GotoXY(40,splitline+1);
                  Write('Endre tider');
                  GotoXY(1,splitline+1);
	          Write('Startnummer :');ReadLn(dummy);
	          IF dummy<>'' THEN BEGIN
	             ReadV(dummy,jj);
                     j:=jj;
	             IF j>0 THEN BEGIN
	                GotoXY(1,splitline+1);
                        ClrBottom;
                        GotoXY(1,splitline+1);
	                Write(j:4,' ');
	                WrL(results[j].name,20);Write(' ');
                        WrHMS(results[j].starttid);Write(' ');
                        WrHMS(results[j].innkomst);Write(' ');
                        WrHMS(results[j].sluttid);
                        WriteLn(' ');
                        Write('Vil du rette   1 Starttid');
                        Write('   2 Innkomsttid   3 Sluttid ? ');
                        ReadLn(dummy);
                        IF dummy<>'' THEN BEGIN
                           ReadV(dummy,jj);
                           j2:=jj;
                           IF (j2>0) AND (j2<4) THEN BEGIN
                              CASE j2 OF
                                 1:BEGIN
                                     results[j].starttid:=GetHMS(results[j].starttid);
                                     IF results[j].innkomst>0 THEN
                                        results[j].sluttid:=results[j].innkomst-results[j].starttid;
                                   END;
                                 2:BEGIN
                                     results[j].innkomst:=GetHMS(results[j].innkomst);
                                     results[j].sluttid:=results[j].innkomst-results[j].starttid;
                                   END;
                                 3:BEGIN
                                     results[j].sluttid:=GetHMS(results[j].sluttid);
                                     results[j].innkomst:=results[j].starttid+results[j].sluttid;
                                   END;
                              END; {* of case *}
                              IF results[j].innkomst=0 THEN results[j].sluttid:=0;
                              IF results[j].sluttid=0 THEN results[j].innkomst:=0;
                              Seek(handle,j-1);
                              Write(handle,results[j]);
                              select:=1;
                              VelgSelv;
                           END;
                        END;
	             END;
	          END;
                  CursOff;
                  GotoXY(1,splitline+1);
                  ClrReminder;
                  ClrReminder;
                  ClrReminder;
                  ClrReminder;
                  ClrReminder;
                  ClrReminder;
                  ClrReminder;
	       END;
{**         70:BEGIN    * F - Full liste *
                  GotoXY(1,6);
                  FOR i:=1 TO 10 DO BEGIN
                     Write(i+respage:4,' ');
                     WrL(results[respage+i].name,20);Write(' ');
                     WrHMS(results[respage+i].starttid);Write(' ');
                     WrHMS(results[respage+i].innkomst);Write(' ');
                     WrHMS(results[respage+i].sluttid);
                     WriteLn(' ');
                  END;
               END;                                   *******}
	    82:BEGIN    {* R - Resultatliste >S-F1*}
                  select:=1;
                  VelgSelv;
               END;
            83:BEGIN    {* S - Startliste >S-F2*}
		  select:=2;
		  VelgSelv;
               END;
            81:BEGIN   {* Q - PageUp *}
                  IF select<>0 THEN BEGIN
                     respage:=respage-listeantall;
                     IF respage<0 THEN respage:=0;
                     VelgSelv;
                     END
                   ELSE BEGIN
                     GotoXY(40,4);
                     Write('Du har ikke valgt noen liste!');
                  END;
               END;
            90:BEGIN   {* Z - PageDown *}
                  IF select<>0 THEN BEGIN
                     respage:=respage+listeantall;
                     IF respage>maxrecords-listeantall THEN respage:=maxrecords-listeantall;
                     VelgSelv;
                     END
                   ELSE BEGIN
                     GotoXY(40,4);
                     Write('Du har ikke valgt noen liste!');
                  END;
               END;
            18:BEGIN   {* Ctrl R - ResPage *}
                  GotoXY(1,22);
                  Write('Respage:');
                  ReadLn(dummy);
                  IF dummy<>'' THEN BEGIN
                     ReadV(dummy,jj);
                     respage:=jj;
                     respage:=respage-1;
                     IF respage<0 THEN respage:=0;
                     IF respage>maxrecords-listeantall THEN respage:=maxrecords-listeantall;
                     GotoXY(1,22);
                     ClrReminder;
                     VelgSelv;
                  END;
               END;
         END; { of CASE ReadKey }
END; {* of PROC SjekkKnapp *}


PROCEDURE Parring;
BEGIN
      IF (nen<>nst) AND (ten<>tst) THEN BEGIN
         j:=nrbuffer[nst];

         tsafe:=results[j].innkomst;
         nsafe:=tst;
         qsafe:=j;
         results[j].innkomst:=tidbuffer[tst];
         results[j].sluttid:=results[j].innkomst-results[j].starttid;
         Seek(handle,j-1);
         Write(handle,results[j]);
         LoparStatus(j);
	 nst:=nst+1;
	 IF nst>buf THEN nst:=1;
	 tst:=tst+1;
	 IF tst>buf THEN tst:=1;
         IF (nen=ten) THEN BEGIN {* Ingen flere -> vis resultater *}
            select:=1;
            VelgSelv;
         END;
      END;
END;

BEGIN
   ClrScr;
   WriteLn(' Resultatsystem for Vallermarsjen ');
   WriteLn('              Skrevet av Henning Spjelkavik, 1993');
   WriteLn('              H & H -Mykvare, 1993 - Valler Skole',chr(10));
   select:=0;
   Results[0].name:='.................................';
   Assign(statlst,LogPathFil);
   Rewrite(statlst);
   WriteLn(statlst);
   Write(statlst,'--> Program Up ');
   WriteLn(statlst,' --> Vallermarsjen ');

   WriteLn(statlst,chr(10));
   ten:=1;tst:=ten;nen:=1;nst:=nen;
   num:=0;
   onr:=0;
   ClrScr;
   GetFileName_ReadFile;
   CursOff;
   Top;
   REPEAT
      GotoXy(1,splitline+1);
      ClrBottom;
      GotoXY(3,4); WrHMS(clock);
      GotoXy(3,6); Write('Tider i k›en',ten-tst:3);
      GotoXY(3,7); Write('St.nr i k›en',nen-nst:3);

      IF KeyPressed=TRUE THEN BEGIN
         f:=ORD(ReadKey);
         SjekkKnapp;
      END; { of KeyPressed }
      Parring;
   UNTIL (f=27);
   WriteLn(statlst);
   Close(statlst);
   If efternavn=4 THEN Close(lst);
END.
