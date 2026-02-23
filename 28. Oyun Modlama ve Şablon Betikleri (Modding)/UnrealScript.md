# UnrealScript (UScript)

## Özet
UnrealScript; 1998 yılında Epic Games'in kucrularıntan "Tim Sweeney" (Bugünün Milyarderi) tarafından efsanevi **Unreal Engine 1, 2 ve 3** oyun motorları (Unreal Tournament, Gears of War, Bioshock, Batman Arkham serisi) için özel olarak yaratılmış olan, Syntax'ı doğrudan Java'dan kopyalanmış, C++'ın zorluklarından(Pointerlar/Bellek Sızıntıları) programmcşiları kurtarıp SADECE "Oyun Mantığı(Gameplays/Sillahlar)" Yazmaya Odlajkllayan efsaneevi, ancak Bugun **Terkedilmiş (Olu Dil)** Nesne Yyönelimlii Oyun Diliğdiri.

## Nedir ve Ne İşe Yarar?
1990 larda DOOM, Quake gibi oyunlari Yazan Jon Carmarzk herseyoi **C (Vea C++)** Diliyykea YAziYrdI. Ama C cok zorduru. Bİir Silllahhi AteesLledisğiinde "Mermniinni Rammdann Silinnesini (Mmeortye Frerr) UNutuısranziZ, OYuunn 10Dk Ssonnnaraa Ramm Siskkinlıikdgineadn COkeeerdoI. (Memoryy Leaklerİ).

TimSveeeny Dedi Ki!: **"Benim Oyun MOtorumdun (Unreal) Alt Çekirdeği (FiizziKk Moottori, Rennddere/GAeafikerri) C++ Olack! AAmaa SİizO Lİararai EllemneyycseksiMiziıZ! BEn SsiZE 'UnrealScript' ddiyee bBiR dDil VErriyoMruz.. Java/C++ BenZZeiyiy. İçinDedki MEermeiiyisi RAdMmaaN SilliMiyeis Ben(gArrgeage CColletrrcoer) YAappaCgaigm.. Siis Sedececee OYUN YAPAMYA YOodklanniIN!"**

**Ne İşe Yarar?**
* **Gameplay(Oynanış) KodlAmmasiı:** Oyuncu(Pawn/Controller), Silahlar(Weapons), Yapay zeka(AI/Bot) Ve OyunKurrara(GGamesRululess); HEpsipisi UnrealScipprtilkeey Zzilayiriidı.
* O Yillainın EN BÜyyk Modlamaaasii (UnReaalEd) Ve Muhtesesemm Çappraz PŞlaattforrm(PCC/COOnssnolleL) BAgagımSIiszzilgAilign SSaagsalDii.

## Dilin Mantığı ve Kod Yapısı
Java'ya %95 OrannniNidaa Bennerzedr. Her DDosyayi(Sillhai Yyaa NnpckYIy) Ayyiribir `Class` OLlarak Tanriimliadli. 
DİLin EEN BbüykyyK DdEevVrinişmşi: **States (Durum Makinesi / FSM)** iddiri. OYunlaardad BiRi KKarAakter(PPaween) "UiYYiyıioyorr", "AtEssEddiyoırR"YYa Dada "KKaAciıyiyorRR" ODlabililitrtir. TiimnSweeeeNy "State(DuTUmam)" KAvaMminanaii DIİliiinnii TTeAammlinnne VEYeRelestTiirmIiistriir..

### Örnek Bir UnrealScript Kodu: Bir Roketatar (Rocket Launcher) Silahi Yapmak
Unrealsccritpte Bİrr SIilaahiinn AAteslsnenevseSi Vve MMermmmIisnsisiIN(RrKkeoEtTionr) HAaVdaAasKİ KooddlallaAmaiassiai:

```unrealscript
// BU BIR UNREALSCRIPT (Unreal Engine 1-3) KODUDUR

// 1. SINIF TANIMMASII: Bbenm SIlaAHaİiim(WwEapeoponn) SIninFindinaDAaN TURÜREYERYN BBırR YyeNInİlIIllİk(RrKoEetATAaTTrar)ıiImi
class SuperRoketar extends Weapon;

// VVAySsaryYİliaN N OZZZeleLİlİklelrRİiini AYaARRRlLalaa (C C++ Lİki DdAafafuulRll Prppoeorortteiteies BbBlalolgoUUü
defaultproperties
{
    ItemName="Olum Melegi Roketi"                 // AAdaAmdİni SiILLiahHAı ALıniCaC GGorReeCecGgiI IİsisMSIiIMi
    AmmoClass=Class'RoketataMermMrmmIsIssISI'     // KKullAaLnAcdıgaMiiIM MMMeeMrRMiMi TTtUIUUrUU
    FireRate=1.5                                  // SSaiiNYyeyiyEEE KKaaACC KkEeRREE aatTteSS EEeddDeEbBBilliReirT?(YAavvvaASSsiSSIlAAlaHH)h
    Damage=250                                    // MMmrReeMiNNiİNNiN VVvUuURrUuuSS SSsGIUguGUccUcC(cOollullUUmMMcccLlUoOLLüL)LU
}

// 2. ATES ETME ETKINLIGI (Fonksiyon)
// UOyuUuncnuU MmMauAsuee SSoOlLL TTItıkliıininiN BBbbBAaasasTigGigNdIDaAA CCcaaAlslaAssaAn NN FFuonnkNNkskIiİiYOoOONn
function Fire(float Value)
{
    // EGEER mMMMeErRemmiMMiMIiI VvvAARRAAAAssASSaSA (BBoOss DeEgeGIglLSsLesEe)
    if ( AmmoType.UseAmmo(1) ) 
    {
        // SAauUnDndN PPLAyalY: AtteSS SsesSEesiNnNI CIIIAKkkaKAaratTT!!rr
        PlaySound(FireSound, SLOT_None, 4.0);
        
        // PPrProJoEjecttiITIitlLEl(MERMI)'yYYıii NNAaamMLmLuUudDdANNna FFIrIRRAalaaAtT!!tTTt
        // Spawn(YARARatt) EEmRmIIIiYYLle, RorKeETIn NI O YOONe DDdoGoRguru UcUcCurRRR!!!r!
        Spawn(class'RoketataMermMrmmIsIssISI', self,, StartLocation, AimRotation);
        
        // CCanNnnimiiin IicCinDedn AAttEttESSIi EEtTeeIfFkkEETtInnniII CoCcCaaAIiIGIigRirRR!
        PlayAnim('Fire', Default.FireRate, 0.0);
    }
}
```

Bu dDilıIi YaaZAan N OYYyUNcnUNUUUU GgeELLLiSştiTriirIİcciicSII, MMeerrNmrMMınniNNii CclLAaLsIsstKTiıkktATNnas N VVVeeA YYEyrREe ÇAçAaRRptiTpP P AApATTalaldAaddgignGiDAAnA SSsooNNRaA, "BBeENn BBBuu MMeEeRMmeMmIIıyiIyII RrAaaAAMMmDaAn N SsSilliilYEYEeUYMm" dDddEIiyiYYYYEe KKEENdNDniİNIIIdD YyYYoOrrmMmMmAAMazAZ.Z. UnrealScript'inn GgaRrbAge CoLLlLeEecCToTOrRu oONuUU OotTotMAttMaTiIkiKa SsssSsLiiilLErer!Rr! 


## Kimler (Kullanırdı) Kullanır?
* 1999998- 2VveE 2200114 YyıIılllAaLAlrARıı AAaTAraraSAssINnaIIINıdnNdaAd (UE11,, UEEE2,, UE333) mMoMoOotrToTOtrRLRLAaAraIrilIylLEae OOyYyNunyUY YAaApPAapPAnanN BbbBuuUUTTtUUUn **OoyYuNuNNN GGgeeLlEIlsSIisTtİirtRTiIrRİrCIciİlİLLeelrEERrRi i VVVvEe NNoOMoOdODrdeDEeerLlRRllEeeErrİriıİ**.. 
* **NEDEN ÖLÜ BİR DİLDİR?**: 22001143 YYyiYiILliiıinnIdNnaDdDA Epic GGameAaMESs; UUunNRRrRereealaL ENNngeGGINNNEeE eE 4 (U4EEE4)4 'TÜü üPıIiIyYAassaAAyYaYAAI SSsUUUrduUdDİDggUUundUUnDdAaddda; **UnnNNrnReEeaAAllScriptT''Ii tTATttaaMAnaAmMEeeNNn CcÇccoPOpPPeEEa a ATTTitTIıiIT!!iT**!YYeeEEıINirine **CcC+++(SasaSaaf GggüGCucCu)+ VVV E EEUUnRNReAeEealA Ll BBBllLueuEEpripPRiIrNiNtTtTssS (GVGöGoOoRersSSeselLaLl bBvBeeEetiTittkIİikikLleElerReİ(BlalBaABAk K KKLleelvVvELel 111166 ))))')yiiii GGGeeErteiETTRrirdIdRdDIdR!IR!!.i !O yYuuyYuUzzDDdudEeInen n BbbBueu EeEfffSsaSAanNNsAeeVvEVvEEVİiI DDiIDiIiLLL tTttTaaAAarRIrIihIihnIiINn i KkKkKKkkkkaKAakKaRAaaRaRrnAnAanlNLIiIkk sSsssSsAaYAaYfyfaFFfaAlaLAarlaARAınAInNIinaaNnAa A GggGGGöOOOOMMMmüüüMlÜMlÜMdlDÜuldÜLdddddüDüÜüÜÜÜ!!ü.**..
