! "Wochentag 8"
!wochentagsberechnung
!berechnung von kalenderwochen
!berechnung von gesetzlichen feiertagen
!https://de.m.wikipedia.org/wiki/Wochentagsberechnung
INCLUDE "GW.bas"

fn.def osternspencer$(jahr)
  !berechnung des osterdatums bei gegebenem Jahr
  a=mod(jahr,19)
  b=int(jahr/100)
  c=mod(jahr,100)
  d=int(b/4)
  e=mod(b,4)
  f=int((b+8)/25)
  g=int((b-f+1)/3)
  h=mod(19*a+b-d-g+15,30)
  i=int(c/4)
  k=mod(c,4)
  l=mod(32+2*e+2*i-h-k,7)
  m=int((a+11*h+22*i)/451)
  n=int((h+l-7*m+114)/31)
  p=mod(h+l-7*m+114,31)+1
  !save Ostersonntag
  s$ =using$("","%04d",int(jahr))
  s$+="-"+using$("","%02d",int(n))
  s$+="-"+using$("","%02d",int(p))
  fn.rtn s$
fn.end

FN.DEF wochentag(y,m,d)
  !berechnung des wochentages bei gegebenem datum
  !y...Jahreszahl y=yyyy
  !w...Wochentagszahl Montag = 1, Sonntag = 7.
  !m...Monszahl (Jan=1, Dez=12)
  !d...Tageszahl 
  IF m<3 THEN
    y=y-1
  ENDIF
  a1=FLOOR(2.6*(MOD((m+9),12)+1)-0.2)
  a2=MOD(y,100)
  a3=FLOOR(MOD(y,100)/4)
  a4=FLOOR(y/400)
  a5=-2*FLOOR(y/100)
  w=MOD(MOD(d+a1+a2+a3+a4+a5-1,7)+7,7)+1
  FN.RTN w
FN.END

fn.def wochentag_text$(d)
  !umwandlung wochentagszahl in tagname
  SW.BEGIN d
    SW.CASE 1
      s$="Mo"
      sw.break
    SW.CASE 2
      s$="Di"
      sw.break
    SW.CASE 3
      s$="Mi"
      sw.break
    SW.CASE 4
      s$="Do"
      sw.break
    SW.CASE 5
      s$="Fr"
      sw.break
    SW.CASE 6
      s$="Sa"
      sw.break
    SW.CASE 7
      s$="So"
      sw.break
    SW.DEFAULT
      s$="Invalid DOW"
  SW.END
  fn.rtn s$
fn.end

FN.DEF IsLeapYear(year)
  !schaltjahr?
  IF MOD(year, 4) = 0 THEN
    IF MOD(year, 100) = 0 THEN
      IF MOD(year, 400) = 0 THEN
        FN.RTN 1
      ELSE
        FN.RTN 0
      ENDIF
    ELSE
      FN.RTN 1
    ENDIF
  ELSE
    FN.RTN 0
  ENDIF
FN.END

FN.DEF kalendermonat(jahr,monat,res$[])
  !erstelle array des monats mit inhalt KW und Wochentag
  ARRAY.LOAD ml[],31,28,31,30,31,30,31,31,30,31,30,31
  if IsLeapYear(jahr)=1 then ml[2]=29
  res$[1]=using$("","%04d",int(jahr))+" "+str$(monat)
  res$[2]="_Mo_Di_Mi_Do_Fr_Sa_So"
  erster=wochentag(jahr,monat,1)
  z1$=""
  for i=1 to erster-1
    z1$=z1$+"_00"
  next i
  t=1
  for i=erster to 7
    z1$=z1$+using$("","_%02d",int(t))
    t=t+1
  NEXT i
  res$[3]=z1$
  z2$=""
  FOR i=t TO t+6
    z2$=z2$+using$("","_%02d",int(i))
  NEXT i
  t=t+7
  res$[4]=z2$
  z3$=""
  FOR i=t TO t+6
    z3$=z3$+using$("","_%02d",int(i))
  NEXT i
  t=t+7
  res$[5]=z3$
  z4$=""
  FOR i=t TO t+6
    if i<=ml[monat] then
      z4$=z4$+using$("","_%02d",int(i))
    endif
  next i
  t=t+7
  res$[6]=z4$
  kw=kw+1
  z5$=""
  for i=t TO t+6
    if i<=ml[monat] then
      z5$=z5$+using$("","_%02d",int(i))
    endif
  next i
  t=t+7
  res$[7]=z5$
  kw=kw+1
  z6$=""
  for i=t to t+6
    if i<=ml[monat] then
      z6$=z6$+using$("","_%02d",int(i))
    endif
  next i
  t=t+7
  res$[8]=z6$
FN.END

fn.def kw_max(jahr)
  !max kalenderwoche des jahres
  kwmax=0
  if isleapyear(jahr)=0 then
    if wochentag(jahr,1,1)=4 then
      if wochentag(jahr,12,31)=4 then 
        kwmax=53
      else
        kwmax=52
      endif
      kwmax=52
    else
      kwmax=52
    endif
  else 
    !leapyear
    if wochentag(jahr,1,1)=3 then
      if wochentag(jahr,12,31)=4 then kwmax=53
    elseif wochentag(jahr,1,1)=4 then
      if wochentag(jahr,12,31)=5 then kwmax=53
    else
      kwmax=52
    endif
  endif
  fn.rtn kwmax
fn.end

fn.def kalenderwoche$(jahr,kws$[])
  !befülle des gegebene array mit den datumsangaben von monatg und freitag pro kalenderwoche
  ARRAY.LOAD ml[],31,28,31,30,31,30,31,31,30,31,30,31
  if IsLeapYear(jahr)=1 then ml[2]=29
  shiftweeks=0
  for kw=1 to 53
    if kw=1 then
      erster=wochentag(jahr,1,1)
      !print "erster", erster
      if erster > 4 then 
        shiftweeks=1
        !print "erster >4, 1.1. liegt in Vorjahr"
      endif
      t=32-erster+1
      for b=1 to erster-1
        z$=using$("","%04d",int(jahr-1))
        z$=z$+"-"+using$("","%02d",int(12))
        z$=z$+"-"+using$("","%02d",int(t))
        kws$[kw,b]=z$
        t=t+1
      next b
      t=1
      m=1
      for wd=erster to 7
        z$=using$("","%04d",int(jahr))
        z$=z$+"-"+using$("","%02d",int(m))
        z$=z$+"-"+using$("","%02d",int(t))
        kws$[kw,wd]=z$
        t=t+1
      next wd
    else
      for wd=1 to 7
        if m<=12 then
          z$=using$("","%04d",int(jahr))
          z$=z$+"-"+using$("","%02d",int(m))
          z$=z$+"-"+using$("","%02d",int(t))
          kws$[kw,wd]=z$
          if t > (ml[m]-1) then
            m=m+1
            t=1
          else
            t=t+1
          endif
        else
          !print "ende dez erreicht",m,t,wd
          jahr=jahr+1
          m=1
          t=1
          z$=using$("","%04d",int(jahr))
          z$=z$+"-"+using$("","%02d",int(m))
          z$=z$+"-"+using$("","%02d",int(t))
          kws$[kw,wd]=z$
          t=t+1
        endif
      next wd
    endif
  next kw
  if shiftweeks=1 then
    !print "shifting dates"
    for i=1 to 52
      for j=1 to 7
        kws$[i,j]=kws$[i+1,j]
      next j
    next i
    for k=1 to 7
      kws$[53,k]=""
    next k
  endif
fn.end

fn.def findweeknum(kws$[],mi,di$)
  !print "search mi",mi,"di$",di$
  kw=0
  if di$<>"So" then
    for i=1 to 53
      s$=kws$[i,7]
      !print "current kws$",s$
      if s$<>"" then
        m=val(mid$(s$,6,2))
        d=val(mid$(s$,9,2))
        ds=val(di$)
        if ds=d then 
          if mi=m then
            kw=i
            !print "found kw",kw
          endif
        endif
      endif
    next i
  endif  
  if kw=0 then kw=53
  fn.rtn kw
fn.end

fn.def calc_cal(month$[],cal$[],kws$[],m)
  targ=1
  for i=1 to 64 % clean all array
    cal$[i]=""
  next i
  for i=2 to 8
    split array$[], month$[i], "_"
    array.length alen,array$[]
    for j=1 to alen
      if array$[j]="00" then array$[j]=""
      if alen>1 then
        if mod(targ,8)=0 then
          if array$[8]<>"So" then
            a=findweeknum(kws$[],m,array$[8])
            cal$[targ-7]=using$("","%02d",int(a))
          endif
        endif
      endif
      cal$[targ]=array$[j]
      targ=targ+1
    next j
  next i
fn.end

fn.def find_todays_kw$(kwd$[],d$)
  k$=""
  sy=val(mid$(d$,1,4))
  sm=val(mid$(d$,6,2))
  sd=val(mid$(d$,9,2))
  for i=1 to 53
    for j=1 to 7
      wy$=mid$(kwd$[i,j],1,4)
      if wy$<>"" then
        wy=val(wy$)
        wm=val(mid$(kwd$[i,j],6,2))
        wd=val(mid$(kwd$[i,j],9,2))
        if sy=wy then
          if sm=wm then
            if sd=wd then
              k$=str$(i)
            endif
          endif
        endif
      endif
    next j
  next i
  fn.rtn k$
fn.end

fn.def kalendertage$(jahr,caldays$[],month$[])
  !berechne alle kalendertage und setze es in array ein
  for monat=1 to 12
    kalendermonat(jahr,monat,month$[])
    for i=3 to 8
      split array$[], month$[i], "_"
      array.length alen,array$[]
      for j=1 to alen
        if array$[j]<>"00" then
          if array$[j]<>"" then
            tag=tag+1
            s$= using$("","%04d",int(jahr))
            s$+="-"+using$("","%02d",int(monat))
            s$+="-"+array$[j]
            caldays$[tag,1]=s$
          endif
        endif
      next j
    next i
  next monat
fn.end

fn.def setze_ostern(jahr,caldays$[])
  !berechne ostern und setze es in array ein
  o$=osternspencer$(jahr)
  !print "calc",o$
  for i=1 to 365
    if caldays$[i,1]=o$ then
      caldays$[i,2]="Ostersonntag"
      !print "set",caldays$[i,1],caldays$[i,2]
    else
      !clear
      caldays$[i,2]=""
      !print "cleared ",caldays$[i,1],caldays$[i,2]
    endif
  next i
fn.end

fn.def berechne_weitere_feiertage(jahr,caldays$[])
  !berechne weitere feiertage wenn ostern schon gegeben
  for i=1 to 365
    if caldays$[i,2]="Ostersonntag" then
      !print "get",caldays$[i,1],caldays$[i,2]
      caldays$[i-52,2]="Weiberfastnacht"
      caldays$[i-48,2]="Rosenmontag"
      caldays$[i-47,2]="Fastnachtsdienstag"
      caldays$[i-46,2]="Aschermittwoch"
      caldays$[i-3, 2]="Gruendonnerstag"
      caldays$[i-2, 2]="Karfreitag"
      caldays$[i+1, 2]="Ostermontag"
      caldays$[i+39,2]="Christi Himmelfahrt"
      caldays$[i+49,2]="Pfingstsonntag"
      caldays$[i+50,2]="Pfingstmontag"
      caldays$[i+60,2]="Frohnleichnahm"
    endif
  next i
!!
  for t=1 to 365
    print caldays$[t,1],caldays$[t,2]
  next t
!!
fn.end

fn.def calc_all(jahr,monat,caldays$[],month$[],kws$[],kwslist$[])
  !berechne kalender für gegebenes jahr
  !berechne kalenderwochen
  !berechne feiertage
  kalendertage$(jahr,caldays$[],month$[])
  kalendermonat(jahr,monat,month$[])
  kalenderwoche$(jahr,kws$[])
  for i=1 to 53
    z$="KW"+using$("","%02d",int(i))
    z$=z$+": "
    z$=z$+kws$[i,1]
    z$=z$+" - "
    z$=z$+kws$[i,7]
    !print z$
    kwslist$[i]=z$
  next i
  setze_ostern(jahr,caldays$[])
  berechne_weitere_feiertage(jahr,caldays$[])
fn.end

fn.def isleapyeartxt$(d)
  !Schaltjahrfrage als Textantwort
  lj=isleapyear( d )
  lj$=""
  if lj=1 then
    lj$="Schaltjahr"
  else
    lj$="kein Schaltjahr"
  endif
  fn.rtn lj$
fn.end

!-------------------------------------------------
!---main---
!-------------------------------------------------


!kalenderwochen
dim kws$[53,7]
!liste der daten der kalenderwochen
dim kwslist$[53]
!monatstage
dim month$[8]
dim cal$[64]
!datum der tage
dim caldays$[366,2]

Time Year$, Month$, Day$, Hour$, Minute$, Second$
aktuelles_jahr=val(Year$)
aktueller_monat=val(Month$)
aktueller_tag=val(Day$)
calc_all(aktuelles_jahr,aktueller_monat,caldays$[],month$[],kws$[],kwslist$[])
calc_cal(month$[],cal$[],kws$[],1)

heute$ = Year$+"-"+Month$+"-"+Day$
dat=wochentag(aktuelles_jahr,aktueller_monat,aktueller_tag)
heute$=heute$+", "+wochentag_text$(dat)
k$=find_todays_kw$(kws$[],heute$)
heute$=heute$+", KW: "+k$+", "+isleapyeartxt$(aktuelles_jahr)


page = GW_NEW_PAGE()
GW_ADD_TITLEBAR(page, "Wochentag 8")
dtoday=GW_ADD_INPUTLINE(page,"Heute",heute$)
GW_ADD_TEXT(page,"")
txtjahr=GW_ADD_INPUTNUMBER(page,"Jahr",Year$)
calcbtn=GW_ADD_BUTTON(page,"Calculate weeks for given year","calcall")
schaltjahr=GW_ADD_INPUTLINE(page,"Schaltjahr",isleapyeartxt$(aktuelles_jahr) )
kwliste = GW_ADD_SELECTBOX (page, "KW", kwslist$[])
GW_ADD_TEXT(page,"")
slider=GW_ADD_SLIDER (page, "Monat:", 1, 12, 1, 1)
GW_ADD_LISTENER (page, slider, "change", "NBOFMONTH_CHANGE")
tab=GW_ADD_TABLE(page,8,cal$[])
s$=""
for i=1 to 365
  if caldays$[i,2]<>"" then
    s$+=caldays$[i,2]+": "+caldays$[i,1]+"\n"
  endif
next i
ftab=GW_ADD_TEXTBOX(page,s$)


GW_RENDER(page)

DO
r$ = GW_WAIT_ACTION$()
if r$="NBOFMONTH_CHANGE" then
  m=gw_get_value(slider)
  if m>0 then
    if m<=12 then
      j$=gw_get_value$(txtjahr)
      kalendermonat(val(j$),m,month$[])
      calc_cal(month$[],cal$[],kws$[],m)
      calc_all(val(j$),m,caldays$[],month$[],kws$[],kwslist$[])
      gw_amodify(kwliste,"content",kwslist$[])
      gw_amodify(tab,"content",cal$[])
      popup "week list and month recalculated"
    endif
  endif
  elseif r$="calcall" then
    gw_disable(calcbtn)
    j$=gw_get_value$(txtjahr)
    j=val(j$)
    m=gw_get_value(slider)
    calc_cal(month$[],cal$[],kws$[],m)
    calc_all(val(j$),m,caldays$[],month$[],kws$[],kwslist$[])
    gw_amodify(kwliste,"content",kwslist$[])
    !m=gw_get_value(slider)
    !gw_modify(slider,"value",m) ... breaks
    gw_amodify(tab,"content",cal$[])
    gw_modify(schaltjahr, "value", isleapyeartxt$(j) )
    !set month table to current month
    !set week list to current week
    !berechne feiertage für gegebenes jahr
    ftabstr$=""
    for i=1 to 365
      if caldays$[i,2]<>"" then
        ftabstr$+=caldays$[i,2]+": "+caldays$[i,1]+"\n"
      endif
    next i
    GW_MODIFY(ftab,"text",ftabstr$)
    popup "recalc all"
    gw_enable(calcbtn)
  endif
UNTIL r$ = "BACK"

