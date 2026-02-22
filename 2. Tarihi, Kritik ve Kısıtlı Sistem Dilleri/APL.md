# APL

## Özet
APL (A Programming Language); 1962 yılında Harvardlı matematikçi Kenneth E. Iverson tarafından, matematik teorilerini "Bilgisayar koduna değil, tahtaya fırçayla yazarmışçasına" ifade etmek üzere icat edilen, evrendeki diğer hiçbir programlama diline benzemeyen, sadece kendine ait  **Yunan harfleri ve garip geometrik sembollerle kodlanan** çılgın ve efsanevi Dizi(Array)-odaklı programlama dilidir.

## Nedir ve Ne İşe Yarar?
1960'larda bilgisayarlara matematiksel bir şey öğretileceği zaman (C, Fortran ile), yüzlerce satır Matrix döndürme, kiralama ve For(Döngü) satırları kodlanıyordu. Kod, asıl düşünülmesi gereken "Matematik formülünü" görsel çöplüğün içine saklıyordu. 

Kenneth Iverson; sadece özel bir klavye (veya bilgisayar programında sanal haritalar) yardımıyla yazılabilen, for/while gibi hiçbir döngüsü olmayan, onun yerine **Rhod (ρ), Iota (ι), Omega (ω)** gibi Yunan/Geometrik karakterlerin ekrana tıklandığı çok yoğun (Dense) bir dil icat etti. "Bir satırda tek sembolle bir milyon satırlık döngünün yapacağı işi çözen dil" olarak tanındı. 

**Ne İşe Yarar?**
* **Ekstrem Sayısal (Quant) Finans / Analiz:** Milyarlarca verilik bir Borsa Grafiğini tek satırda filtreleyip sonucunu bulmak... Wall Street'in zamanındaki gizli (hala da Niş) aracıdır. Çünkü APL "Array-Processing (Dizi İşleme)" yeteneğiyle döngüsüz çalışır, C dilinde 50 sayfa olan bir borsa analizini APL ile 30 Karakterlik (1 satır) acayip sembollerin birleşimiyle yazabilirsiniz!
* Diğer dillere J ve K dillerine ilham verirken günümüzde "Kdb+ Veritabanının" Wall Street bankalarındaki devasa finansal veri mimarisine arka planda yol göstermiştir.

## Dilin Mantığı ve Kod Yapısı
Bu dili anlamak İngilizce bilmekle alakalı DEĞİLDİR, Matematik Sembolizmi okumayı bilmekle alakalıdır. APL koduna bakan normal bir yazılımcı "Bu hiyeroglif mi, dosya mı bozulmuş?" tepkisini verir. 

Kesinlikle ama kesinlikle **Döngü (Loop / For / While)** kullanılmaz. R ve MATLAB'in ruhsal atasıdır; siz bir sembol çizersiniz, APL o formülü otomatikman Milyonlarca Dataya aynı anda saldırarak enjekte eder. Değişken ataması eşitlikle değil ok sembolüyle `←` yapılır. Sağdan sola doğru (Arapça gibi ama matematiksel öncelikle) okunup işleme dizilir. Tüm Operatörlerin (çarpım/toplam) matematiksel önceliği eşittir!

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin, 1'den X'e kadar tüm sayılardan **Sadece Asal Olanları Bulan** bir program, Java'da en az 25 satırlık sınıf/döngü algoritması ister. APL diliyle bu devasa zor algoritma, şaka değil; KLAVYEDEN ŞU 11 SEMBOLÜ YAN YANA BASARAK gerçekleşir:
`(~R∊R∘.×R)/R←1↓⍳R` 

### Örnek Bir APL Kodu: Sembollerle Dizi İşleme "Hiyeroglifleri" 
Bu dili klavyende yazamayacağınız için özel bir APL idesi (Dyalog APL vb.) kullanılır. Matrislerin o sembolik dili:

```apl
⍝ APL dilinde yorumlar Lambe sembolünün ucuna konan isaretle ⍝ başlar!
⍝ Bu kodlar normal klavyeyle değil "APL Klavyesi" font destegiyle basilir. Asagi ok (←) vb.

⍝ 1'den 10'a KADAR BIR DIZI YARAT VE (İyota - ι) ile Değişkene Ata!
⍝ "DİZİ eşittir, say 10'a kadar" anlami tasir:
DİZİM ← ι 10 

⍝ Ekrana yazilim (sadece degiskeni cagirinca Konsola 1 2 3 4 5 6 7 8 9 10 duser)
DİZİM


⍝ === DÖNGÜSÜZ (NO-LOOP) DİZİ MUCİZESİ (ARRAY PROGRAMMING) ===

⍝ Eger butun sayıların "5 Fazlasının Karesini" firlatmak istersek? (C dilindeki For dongulerine YENILIN!)
⍝ APL diyor ki, dümdüz matematigi yaz!
SONUC ← (DİZİM + 5) * 2

⍝ Dizi anında (6 7 8 9...'dan baslayarak) Çarpanla vurulur!


⍝ === ŞİZOFRENİK ZEKAYA BİR BAKIŞ: MATRIX (2 BOYUTLU TABLO) YARATMAK ===
⍝ Rho (ρ) sembolü sekillendirme (Reshape) harfidir.
⍝ Diyoruz ki; 3 satır 4 sutun'luk bir Matris Çiz (3 4 ρ), İcindeki Sayılar 1'den 12'ye kadar olsun (ι 12)

TABLOMUZ ← 3 4 ρ ι 12

⍝ Tablomuz cagirildiginda, Konsolda(Ekranda) saniyesinde cıkan Gorsel Sanat sudur:
⍝  1  2  3  4
⍝  5  6  7  8
⍝  9 10 11 12

⍝ VE TABLO SATIRINI TOPLA (+/):
+/TABLOMUZ
⍝ Cıktı (Döngüsüz Her Satirin Toplami): 10 26 42 
```
Bütün bir devasa banka matrisini veya bir hava yolu şirketinin müşteri algoritmasını, işte bu sembollerden tek bir satıra sıkıştırarak fırlatan dil.

## Kimler Kullanır?
* Eskiden Finansal "Derivatives (Türev) Fiyatlama", Aktüerya hesaplamaları veya IBM 360 Sistemlerinde "Çılgın Profesörlerin Akademik Makale Hesapları" için yaygındı.
* Bugün neredeyse kimse. Ancak Arthur Whitney ismindeki dâhi, bu dilden ilham alarak "A(ASCII)-APL" diyebileceğimiz K dilini yaratmış, o da dünyadaki çok uluslu dev bankaların arka planda milyarlarca dövizi eşzamanlı takas etmesini sağlayan `Kdb+` veritabanı motorunun ta kendisi olmuştur. Ruhu yaşamaktadır.
