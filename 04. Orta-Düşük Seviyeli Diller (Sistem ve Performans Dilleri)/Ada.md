# Ada

## Özet
Ada; 1980'lerde ABD Savunma Bakanlığı (DoD) tarafından yaratılan, dünyadaki **en güvenilir, arıza toleransı (Fault Tolerance) tarihte kanıtlanmış bir şekilde sıfır olan** görev-kritik (mission-critical) gömülü sistem programlama dilidir. Adını programlamanın teorik ilk kurucusu Ada Lovelace'den alır.

## Nedir ve Ne İşe Yarar?
1970'lerde Amerikan ordusunun envanterindeki tanklar, füzeler, denizaltılar ve radarların her biri farklı donanımlarla, 450'den fazla farklı alt sınıf programlama diliyle devasa bir kaos ordusu şeklinde çalışıyordu. Sistemlerin birbirleri ile çökmeden bütünleşik entegre çalışması imkânsızdı. 

Savunma birimleri "Öyle bir dil yapın ki; hatayı daha derleme aşamasında fırlatıp patlatsın, çalışırken asla ekranı dondurmasın ve sonsuza dek sağlam sistemler garanti etsin" diyerek uluslararası bir yarışma başlattı, bu yarışmadan **Ada** doğdu.

**Ne İşe Yarar?**
* **Sıfır Hata Toleranslı (Safety-Critical) Gömülü Sistemler:** Bir e-ticaret sitesi kodunda hata çıkarsa (Örn: C veya JavaScript ile yazılmış) sepetiniz donar. Uçağın (Boeing 777 veya Airbus) otopilot sisteminde hata çıkarsa (Memory taşması vs.) yüzlerce insan ölür. Nükleer reaktörlerde sızıntı kontrolünde mavi ekran veremezsiniz. Uzay mekikleri çakılamaz. İşte tüm bu sivil ve askeri hayat-memat meseleleri dünyada standart olarak **Ada (veya Spark alt türevi)** ile yazılır.
* **Katı Matematik ve Eşzamanlılık (Concurrency):** Füze havada 3000 km hızla giderken aynı milisaniyede 15 sensörden (hava, ivme, ısı) veri okunup işleniyorsa, dildeki süreç senkronizasyonu hatasız olmalıdır (Ada'daki 'Tasking' mekanizması).

## Dilin Mantığı ve Kod Yapısı
Ada, kelimenin tam anlamıyla bir "Katı Tipli (Strongly Typed)" dildir. Bir değişkene `Elma` türü verip sonra ona `Armut` sayısını (sayısal olarak aynı büyüklükte dahi olsa) "zorla çevirip" atayamazsınız, dil reddeder.

Örneğin, C'de kilometre olan bir sayıyı litre olan bir hız fonksiyonuna atarsanız komut çalışır (ve Mars Climate Orbiter görevi fırlatışındaki gibi roketin milyon dolar olup infilak etmesine yol açar). Ada, değişkenlerin değer sınırlarını kesin hatlarla böler: "1 den 10'a kadar geçerli olan basınç rakamları" diye bir tip uydurursunuz, eğer o değişkene 11 değeri gelirse program anında bunu sistem güvenliği subabına fırlatır.

**Örnek İşleyiş (Sembolik Olarak):**
Sözdizimi çok konuşkandır (verbose). Süslü parantezler yoktur (C gibi diller bu parantezler unutulduğunda facia çıkarır). `if`, `end if;` gibi kompasın başını ve sonunu insan dilindeki gibi netçe kapatır. "Aman şu satırı kısa yazarak zamandan kazanayım" diye bir derdi yoktur, "Sabaha kadar kod yazayım ama 10 yıl bu makinede uzayda dağ başında bir kere bile donmadan çalışsın" felsefesi vardır.

### Örnek Bir Ada Kodu: Sıkı Sınır (Range) Kontrolü
Havacılıkta bir uçağın motor türbin ısısını ölçen ve sadece gerçekçi rakam aralığını geçerli kılan bir yazılım iskeleti. Sensör sahte bir değer yollarsa C'deki gibi saçma sapan işlem yapmaz, hata (Exception) yönetimine başvurur:

```ada
-- Standart metin ekran ciktisi kütüphanesi
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Ucak_Motor_Kontrol_Sistemi is
    -- ADA'NIN GÜCÜ: Tüm tipleri "Kesin Sınırlar" ile programcı belirler
    -- Motor isisi gercek fiziksel dunyada -50 dereceden asagi, 1200'den yukari olamaz.
    type Motor_Isisi is new Integer range -50 .. 1200; 

    -- Bu tipte bir degisken urettik
    Anlik_Sicaklik : Motor_Isisi;

begin
    -- Varsayalım ki sensörden normal bir okuma aldık
    Anlik_Sicaklik := 850; 
    Put("Motor Isisi Normal Seviyede: ");
    Put(Integer(Anlik_Sicaklik));
    New_Line;

    -- SENSÖR HASAR GORDU (Kisa devre yapti veya sahte veri verdi)
    -- Asagidaki satir DERLEYİCİ DE KABUL ETSE, ÇALIŞIRKEN ANINDA GÜVENLİĞİ DEVREYE SOKAR:
    -- C dili olsa bunu 1500 kabul edip uçağı belki patlatacak hesaplar yaptiracakti:
    
    Anlik_Sicaklik := 1500; -- Sistem sinirini(1200) astigi icin Constraint_Error firlatir

    Put("Bu yazi asla gorunmeyecek cunku sistem ustte tetiklendi.");

-- Güvenlik Subabı (Exception Handler): Hata yakalandigi an Otopilot ne yapacak?
exception
    when Constraint_Error =>
        Put_Line("!!! ACIL DURUM !!!");
        Put_Line("Sensör Verisi Fiziksel Sınırlar Dışında! Yedek Sensöre Geçiliyor!");
end Ucak_Motor_Kontrol_Sistemi;
```
Böylece kod asla sessizce hata yapmaz, sınır (range) kontrolü o sistemin DNA'sındadır.

## Kimler Kullanır?
* Uluslararası Savunma Sanayii mühendisleri (Lockheed Martin, Airbus, Boeing), TSK sistem altyapıları, roket ve balistik sistem yazılımcıları.
* Demiryolu ağı sinyalizasyon merkezleri (Fransa yüksek hızlı TGV trenleri baştan aşağı Ada ile yönetilir).
* Uydu programlayıcıları (ESA/NASA dahil).
