# TLA+ (Temporal Logic of Actions)

## Özet
TLA+ ; 1999 yılında Turing Ödülü sahibi Efsanevi Bilgisayar Bilimcisi **Leslie Lamport** (Aynı Zamanda LaTeX sisteminin 'La' kısmının Yaratıcılarındandır!) tarafından icat edilen; Dünyadaki devasa dağınık sistemlerde (Örneğin Amazon'un Kıtalararası Sunucu Sistemleri), Kodu Çalıştırmadan BİLE **"Kodun Altyapısı / Mimarisinin (Dizaynı) İçerisinde Evrenin Çökebileceği Paralel (Eşzamanlılık) Zaman Hataları (Race Conditions) ve Kilitlenmeler(Deadlocks) Var Mı"** sorusunu; Matematiksel Kümeler teorisi ve Zaman Mantığı (Temporal Logic) kullanarak tarayan, Mimarilerin Çöküş İhtimalini Yok eden Muazzam Bir **Model Denetleyicisi ve Tasarım (Specification) Dilidir**.

## Nedir ve Ne İşe Yarar?
Diyelim ki C++ ile veya Java ile bir Program (Veritabanı Senkronizasyonu) Veya Blockchain Ağı yazdınız. Aynı ANDA (Milisaniyede) Müşterilerin 2 tanesi Para Çekmeye Basarsa ne olacağını TEST edebilirsiniz(!). Ancak sisteminiz 1 Milyar Kullanıcıyla Büyüyüp 200 Sunucu arasına Dağıldığında (Distributed Systems); 3 Numaralı Cihaz, 8 Numaralı Makineye Komut Gönderirken **Tam Saniyenin 1/Trilyonu hızında Kesintiye (Elektrik vs) uğrarsa Algoritmalar Nasıl Bir ATEŞE düşer?** Bu "Beklenmedik Zaman (Concurrency) Hataları"; Milyonda Bir çıkar Ama çıktığında Sistemi Havaya Uçurur. (Bknz: Kriptopara Hackleri, AWS çökmeleri).

Leslie Lamport Derki: **Kodlama bir Eziyettir. Biz KODU Yazmadan Önce; Algoritmanın Tasarımını "Tahtada MATEMATİKSEL MANTIKLA" Dizeceğiz!**
Siz Sistemin Tasarımını (Mesela Bir ATM Para Çekme algoritmasını) TLA+ formülüne dökersiniz. Arkasındaki Model Denetleyici (TLC Motoru), Sizin algoritmanızın "Bütün Olası Evrenlerini (State Machine)" Açar ve "Bak Bak, Şurada Aynı Milisanide 2 Kişi Basarsa VE Veritabanı Çökerse Sayfalar Boş Gelebilir" deyip BUG'ı YÜZÜNÜZE TOKAT GİBİ VURUR.

**Ne İşe Yarar?**
* **Amazon Web Services (AWS) - DynamoDB:** Dünyanın İnternetini(Bulutunu) tutan Amazon, 2011'lerde kendi iç Altyapılarını TLA+ Motoruna Soktuğunda "Yıllarca Hiç Kimsenin Fark etmediği", Üstelik Aylar Süren Unit/Milyon Trilyon Testlerde (QA) ASLA Ortaya Çıkmayan **Devasa Gizli Dizayn Hataları** Bulunup Sistemler O Hatalardan Kurtarılmıştır!
* Havacılık / Otomotiv (Rover Araçları): Yazılım Mimarları TLA+ yi C# Coderlara vermez; "Mimarlara" verir. Arkadaş Senin Tasarımın Daha Baştan Hatalı! Demek İçin.

## Dilin Mantığı ve Kod Yapısı
Çizimler, Mantık Kapıları (VE/VEYA) ve Küme teorisidir(Set Theory). 
Çok Aşırı Akademiktir. "Zaman İçindeki Gidişat (Next State)" Kavramı TLA+ nın Kalbidiri (Temporal Logic).

Süslü Oklar, `=>` (Implies - Şunu Gerektirir), `/\` (Mantıksal VE - AND), `\/` (Mantıksal VEYA) gibi Sembollerle Kağıt üzerindeki Filozof Fikirleri İnşa edilebilir.. TLA+'nın Kendi Bir IDE'si (Arayüzü/Aracı) Vardır ve Kod Yazıp O IDe'nin Gözüne (TLC Cihazına) atarsınız.

### Örnek Bir TLA+ Spesifikasyonu: İki Tane "İş Parçacığının" (Yazılımın) Veritabanında İhtilafa (Race Condition'a) Düşmeden Aynı Sayıyı Düzenleyebilme Matematiği Tasarımı!

Aşağıda Bir Para Transferinin Kilitleyici Tasarımı (TLA+ Spesifikasyonu Formülize edilmişi) Görülmektedir:

```tla
----------------------- MODULE ParaTransferSistemi -----------------------

EXTENDS Integers  \* Integer(Tam Sayı) Kütüphanesini Çek, Matematiğe Başlıyoruz!

(* DEGISKENLERIN (Sistemin Durumları) BEYANI *)
VARIABLES bakiye_Ali, bakiye_Veli, pc  
\* pc = (Program Counter - Makine o an İşlemin Hangi Aşamasında (State'inde))


(* 1. INIT(ILK DURUM): SİSTEMİN BAŞLANGIÇ KURULUMLARI (Cihaz ilk Açıldığında Ne Halde?) *)
Init == 
    /\ bakiye_Ali = 100               \* Alinin Başlangıçta 100 Doları var  (DİKKAT!! /\ İşaretleri 'Mantıksal VE' Demektir!)
    /\ bakiye_Veli = 50               \* Velinin Başlangıçta 50 Doları Var
    /\ pc = "Başlangıç"               \* Sistem Şu an Başlangıçta Uyumada Duruyor.


(* 2. OPERASYON / EYLEM (Action): Ali'den Veliye 30 Dolar Yollanması Gidişatı *)
TransferOperasyonu == 
    /\ pc = "Başlangıç"               \* ŞART: Bu İşlemi Yapması için Programın O an Başlangıç Durumunda (Firesiz) Olması Gerekir!
    /\ bakiye_Ali >= 30               \* ŞART 2: Alinin İçerdeki parası Mutlaka ama Mutlaka 30'dan veya Büyük Olabilsin! (Eksiye Düşmesin!)
    /\ bakiye_Ali' = bakiye_Ali - 30  \* DEĞİŞİM (Kesme İşareti ( ' ) 'Next State/Gelecek Zaman' demektir!!. Yani ALİNİN GELECEKTEKİ PARASI = Eski Parasından 30 eksiltilmiş OLACAKTIR!!)
    /\ bakiye_Veli' = bakiye_Veli + 30 \* Velinin Gelecekteki Yeni Parası == Eski parasına 30 Eklenmesidir.
    /\ pc' = "Transfer_Bitti"         \* Gelecekteki Zamanda Programın Durmuş Hali ("Transfer_Bitti") Olarak Değişir!


(* 3. GELECEK(NEXT STATE) TANIMLAMASI : Bütün Sistemin Ne Yöne Akacağının Belirtilmesidir *)
Next == 
    \/ TransferOperasyonu  \* Ya Bu Transfer İşlemi Uygulanır (Mantıksal VEYA - \/ sembolü)
    \/ (pc = "Transfer_Bitti" /\ UNCHANGED <<bakiye_Ali, bakiye_Veli, pc>>) \* VEYA, İşlem zaten BİTMİŞTİR Ve Sistem Daha da Hareket Etmez (UNCHANGED Donar Kalır).


(* TEMEL SPESİFİKASYON (Şartname - Kapsayıcı Felsefe) - SİSTEMİN TÜM YÜKÜMLÜLÜĞÜ *)
====> İlk Durumla(Init) Başlar, Ve Zaman (Temporal Ok ->) İçinde "Next" İşlemleriyle Devam eder !!
Spec == Init /\ [][Next]_<<bakiye_Ali, bakiye_Veli, pc>>

--------------------------------------------------------------------------
```

Bu kod "TLC Model Denetleyicisi"ne Verildiğinde; TLC Motoru bütün zamansal olasılıkları ve paralel (3 kişi aynı anda girdi) ihtimallerini (Milyarlarca ihtimal) 2 dakikada açacak. Eğer sistem çökmüyorsa "TASARIMIN KUSURSUZ (Deadlock YOK)!" diyecek. Eğer çökerse sana yol haritası çıkartacak "Ali aynı saniyenin trilyonda birinde basarsa Veli de basarsa paranın buharlaşarak kaybolduğunu tespit ettim!!" diyecek!
Sen Tasarımını (Spesifikasyonunu) düzelttikten sonra kodcuları (C++/Java'cıları) çağırıp; "BEYLER BU FORMÜLÜN GERÇEĞİNİ KODLAYIN YIKILAMAZ BİR KALE KURDUK" diyeceksin!

## Kimler Kullanır?
* C/C++/Java Geliştirici Coderların Başında Durup O Kodların "Yol ve Dizayn Haritasını Çizen" Yüce Dağlardaki **Distributed Systems (Dağıtık Sistemler) Kademeli (Senior/Staff Yöneticisi) Yazılım Mimarları, Akademisyenleri, Microsoft (Azure/Xbox Çekirdeği) ve Amazon (Bulut Altyapısı)**. Maliyetten Kaçaraktan "Biz Tasarıma İhtiyaç Duymuyoruz" diyen Kurumun, Günün Sonunda Buluttaki Patlaklarla Tırmalayan Acısını Çözen; Milyon Dolarlık Hataları 3 Sayfalık "Felsefi (TLA Formülleri)" yle İptal Eden, Dünyanın En Zekice ve En Zor Sanatlarından Biridir.
