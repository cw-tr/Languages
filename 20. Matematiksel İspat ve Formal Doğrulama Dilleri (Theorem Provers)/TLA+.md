# TLA+ (Temporal Logic of Actions)

## Özet
TLA+ ; 1999 yılında Turing Ödülü sahibi Efsanevi Bilgisayar Bilimcisi **Leslie Lamport** (Aynı Zamanda LaTeX sisteminin 'La' kısmının Yaratıcılarındandır!) tarafından icat edilen; Dünyadaki devasa dağınık sistemlerde (Örneğin Amazon'un Kıtalararası Sunucu Sistemleri), Kodu Çalıştırmadan BİLE **"Kodun Altyapısı / Mimarisinin (Dizaynı) İçerisinde Evrenin Çökebileceği Paralel (Eşzamanlılık) Zaman Hataları (Race Conditions) ve Kilitlenmeler(Deadlocks) Var Mı"** sorusunu; Matematiksel Kümeler teorisi ve Zaman Mantığı (Temporal Logic) kullanarak tarayan, Mimarilerin Çöküş İhtimalini Yok eden Muazzam Bir **Model Denetleyicisi ve Tasarım (Specification) Dilidir**.

## Nedir ve Ne İşe Yarar?
Diyelim ki C++ ile veya Java ile bir Program (Veritabanı Senkrozonisyonu) Veya Blockchain Ağı yazdınız. Aynı ANDA(Milisaniyede) Müşterilerin 2 tanesi Para Çekmeye Basarsa ne olacağını TEST edebilirsini(!). Ancak sisteminiz 1 Milyar Kullanıcıyla Büyüyüp 200 Sunucu arasına Dağıldığında (Distributed Systems);  3 Numaralı Cihaz, 8 Numaralı Makineye Komut Göndeririken **Tam Saniyenin 1/Trilyonu hızında Kesintiye (Elektrik vs) uğrarsa Algoritmalar Nasıl Bir ATEŞE düşer?** Bu "Beklenmedik Zaman (Concurrency) Hataları"; Milyonda Bir çıkar Ama Çıkdığında Sistemi Havaya Uçurur. (Bknz: Kriptopara Hacklarl, AWS cökmeleri).

Leslie Lamport Derki: **Kodlama bir Eziyettir. Biz KODU Yazmadan Önce; Algoritmanın Tasarımını "Tahtada MATEMATİKSEL MANTIKLA" Dizecegiz !**
Siz Sistemin Tasarımını (Mesela Bir ATM PArA Cekme algoritmyasını) TLA+ formülüne dökersininz. Arkasındaki Model Deneteyeci (TLC Motoru), Sizin algoritmanızın "Bütün Olası Evrenlerini (State Machine)" Açar ve "Bak Bak, Şurada Aynı Milisanide 2 Kisi Basaersa VE Veritbnaı Cöekerse SAyfalat Boş Gelebirili" diyip BUGI YÜZÜNÜZE TOKAT GİBİ VURUR.

**Ne İşe Yarar?**
* **Amazon Web Services (AWS) - DinamoDB:** Dünyanını İnternetini(Bulutunu) tutam Amanzon, 2011 lerde kendi iç Altyapılarını TLA+ Motruna Soktuğunda "Yıllaca Hiç Kİsmenin Fark etmediği", Üstelik Aylar Süren Unit/Milyon Trilyon Tesstlerde (QA) ASLA Ortaya Şıkamayn **Devasa Gizli Dizayn Hataları** Bulnups Sistemler O Htalardan Kuratırlnmıştır!.
* Havacılık / Otomativ (Rover Araçları): Yazılım Mimarları TLA+ yi C# Coderlara vermez; "Mimarlara" verir. Arkmadas Senin Tasarımın Daha Başatan Hatalı! Demek İçni.

## Dilin Mantığı ve Kod Yapısı
Çizimler, Mantık Kapıları (VE/VEYA) ve Küme teorisidir(Set Theory). 
Çok Aşırı Akademiktir. "Zaman İçindeki Gidişat (Next State)" Kavramı TLA+ nın Kalbidiri (Temporal Logic).

Süslü Oklar, `=>` (Implies - Şunu Gerektiriir), `/\` (Mantıksal VE - AND), `\/` (Mantksal VEYA) gibi Sembollerle Kağıt uzerindeki Filozof Fikirleri İnşa edlierilir.. TLA+ nın Kendoi Bir IDE si (Arauzu/Araci Vardir ve Kod Yazip O İdnein Güzüne(TLC Cihaziinaa) atarsınız.

### Örnek Bir TLA+ Spesifikasyonu: İki Tane "İş Parçacığının" (Yazılımın) Veritabaınada İhtilafa (Race Conditiona) Düşmeden Aynı Sayıyı Duzenleybilme Matematigi Tasarimi!

Aşağıda Bir Para Transfşerinin Kitleyici Tasarımı (TLA+ Spesifikysyn Formülze eedemişi) Görüülmektedir:

```tla
----------------------- MODULE ParaTransferSistemi -----------------------

EXTENDS Integers  \* Integer(Tam SAyi) Kutuphanresii Cek, Matematige Basliyoruz!

(* DEGISKENLERIN (Sistemin Durumnlari) BEYANI *)
VARIABLES bakiye_Ali, bakiye_Veli, pc  
\* pc = (Program Counter - Makine o an Isllemdin Hangi Asamasinsa (Sateinde))


(* 1. INIT(ILK DURUM): SİSTEMİN BASLANGIC KURULUMLARI (Cihazilk Actlııgında Ne Halde?) *)
Init == 
    /\ bakiye_Ali = 100               \* Alinin Baslangicta 100 Dollari var  (DİNKAT!! /\ İSaretleri 'Mantıksal VE' Demektir!)
    /\ bakiye_Veli = 50               \* Veliniin Basşlangicta 50 Dolari Var
    /\ pc = "Başlangıç"               \* Sistem Su an Başangcta Uyumada Duruyor.


(* 2. OPERASYON / EYLEM (Action): Ali'den Veliye 30 Dolar Yollanmasi Gidişatti *)
TransferOperasyonu == 
    /\ pc = "Başlangıç"               \* ŞART: Bu İslemi Ypamasi icn Programin O an Bashlaginc Durumuda(Firesiz) Olmması Gerekir!
    /\ bakiye_Ali >= 30               \* ŞART 2: Alinin İçerdeki pariasi Mutalaka ama MOtlka 30 Dah veYa Buyuk Olanbilsin! (Eksiye Dussmesisn!)
    /\ bakiye_Ali' = bakiye_Ali - 30  \* DEGİSİM (Kesme İšareiti( ' ) 'Next State/Gelecek Zanman' demekkttir!!. Yani ALİNİN GELECEKTEKİ PARASİ = Eskii Parasindan 30 eksiltilmis OLACAKTIR!!)
    /\ bakiye_Veli' = bakiye_Veli + 30\* Velininin GElckteki Yebni Parsi == ESki parssina 30 Elkemnsidir.
    /\ pc' = "Transfer_Bitti"         \* GEllcktieiki Zamnada Prgormanin Durmus Halaii ("Trafnser Bttit)" Oloarak Dgisir!


(* 3. GELECEK(NEXT STATE) TANIMLAMASI : Bütün Sistsein Ne Yöne Akacanigin BElrtiemsidiri *)
Next == 
    \/ TransferOperasyonu  \* Ya BU Transfer İšlemi Yaslanir (Mantıksal VEYZ - \/  sembolu)
    \/ (pc = "Transfer_Bitti" /\ UNCHANGED <<bakiye_Ali, bakiye_Veli, pc>>) \* VEYSDA, İşlkmem zatte BİTMİŞTİ R Ve Sistem DAha da Harkeket Etmez(UNCHANGED Donar Kalir).


(* TEMEL SPESYFIYKSAON(Sartname - Kapsayucu Felseefe) - SİTEMİN TÜM YUKUMLLLUGU *)
====> İLk Durumla(Init) BAslar, Ve Zamzn(Teomproal Ok- >) Icinde "NExt" İslemelriyle Devmm edeer !!
Spec == Init /\ [][Next]_<<bakiye_Ali, bakiye_Veli, pc>>

--------------------------------------------------------------------------
```

Bu kod "TLC Model Deneteyicisi"Ne Verildiğinda; TLC Mototru Butun Zamansal Olaszılkrı Ve Pararlel (3 Kisi aynna ANda Girdi) ihtimlareinii (Milyoralraca İhtimal) 2 Dnkkada Acacaek. Eeğer Sistem Cokmuyosa "TASAIIMIN KUSURSUZ (Deadlocok YOK)!" Diyeck. EGe Cökerseb sana Yol Haritisai cikaartackt "Ali Aynnanda SAaniyernun trilonuda 1inde Basarss Veli de  Basara Para Buharlasararak Kaybolduğunu Tespti EttiTimm!!" Diycek!
Sen Tasraminu(Speckfikoyasnnu) düzellttiken SOnra Kodcyaral (C++/JAva cularraı) Cagirip; "BEYLER BU FORMULLÜN GERÇEĞİNI KODLAYIN YIKILAMAZ BIR KALE KURYTUK " Diueckeisin!

## Kimler Kullanır?
* C/C++/Java Geliştirici Coderların Başinda Durup O Kodların "Yol ve Dizayn Haritasını Çİzen" Yüce Dağlardaki **Distrubted Systems (Dagıtık Sistemler) Kademeli (Senir/Staff Yöneticisni) Yazılım Mimari, Akademisyenleri, Microsoft(Azzure/Xbox Cekirdegi) Ve Amzon (Bulut Altyapsii)**.  Maliyeten Kacaraktan "Biz Tasarima İhtiyacc Duymuruyiz" diyen Kurumun, Gününn Sonuna Buluktakin Patlaklarka Tirmıalyan Acısını Çözen; Mİlyon Dolarlık Hatalrı 3 Sayffaliıik "Felseffi(TLA Formulleriu)" yle İptal Endem , Dünyanini EN Zekice ve EN zor Sanatlarından Biridir..
