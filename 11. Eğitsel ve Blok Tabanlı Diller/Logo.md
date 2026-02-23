# Logo

## Özet
Logo; 1967 yılında Wally Feurzeig, Seymour Papert ve Cynthia Solomon tarafından icat edilen, evrendeki ilk "Çocuklar/Öğrenciler İçin Eğitsel Programlama" dili olan, Lisp felsefesini barındırmasına rağmen hafızalara ekranda (veya gerçek dünyada) gezinen **"Kaplumbağa (Turtle Graphics)"** ve o kaplumbağayı komutlarla yürüterek çizim(Geometri) yapma efsanesiyle kazınmış kült eğitim dilidir.

## Nedir ve Ne İşe Yarar?
1960'larda bilgisayarlar askeri/bilimsel devasa cihazlardı ve matematiksel işlem basıyorlardı. MIT laboratuvarı (Seymour Papert, Jean Piaget'in eğitim felsefesinden etkilenmişti) çocukların sadece pasif bir şekilde bilgiyi almak yerine, "Bilgisayara bir şeyler inşa ettirerek (Constructivism)" matematiği ve açıyı kendi kendilerine öğrenebileceklerini savundu.

Böylece Logo dili doğdu. Ekranın ortasında bir "Kaplumbağa" (Turtle) vardı. Çocuğa dediler ki: "Döngü ve sayıları kullanarak bu kaplumbağaya bir Kare çizdir!". Çocuk da `İLERİ 100`, `SAĞA 90` yazmaya başladı. Çocuklar kendi kendilerine 360 derecenin, döngülerin, içiçe algoritmaların piri oldular.

**Ne İşe Yarar?**
* **Geometri ve Algoritma Eğitimi:** Logoda matematik kağıt üstünde ezberlenmez. Bir eşkenar üçgen çizmek için kaplumbağanın 120 derece dönmesi gerektiğini (Dış açı kuralı) çocuk ekranda deneyerek ("Aa, 60 derece dönünce üçgen kapanmadı!") öğrenir.
* **Turtle Graphics (Kaplumbağa Grafiği):** Daha sonra C, Python (Turtle modülü), Swift ve diğer dillere taşınmış ve standartlaşmış o eşsiz "Çizgi Çizen Uç" (Pen-Plotter) felsefesini doğurmuştur.

## Dilin Mantığı ve Kod Yapısı
Tam bir Lisp diyalektiğidir. Ancak parantez eziyeti yerine çocukların okuyabileceği Amerikan İngilizcesi cümlelerinden esinlenmiştir. İlerleyen yıllarda pek çok dile (Türkçe Logo vb.) çevrilmiştir.

- **Hareket Komutları:** `FORWARD` (Veya `FD`), `BACK` (`BK`), `RIGHT` (`RT`), `LEFT` (`LT`).
- **Çizim Araçları:** `PENUP` (PU - Kalemi kaldır, yürü ama çizme), `PENDOWN` (PD - Kalemi bas ve çiz).
- **Döngüler:** En meşhur komutu `REPEAT` (Tekrarla).

**Örnek İşleyiş (Sembolik Olarak):**
Ekrana 100 piksel git ve Doksan derece Sağa Dön: `FD 100 RT 90`

### Örnek Bir Logo Kodu: Ekranda (Algoritma Satarak) Kare Çizdirmek
Bir çocuğun kendi ilk "Döngüsünü (Loop)" keşfettiği felsefi anı ve prosedürel fonksiyon (Kendine has kelime(TO)) tanımlama mantığı:

```logo
; Logo dilinde Yorum Satiri Noktali Virgul (;) İle saglanir

; 1. KENDİ FONKSİYONUNU(KELIMENI) ÜRET (Prosedur Tanımlama)
; Eger ileride sadece 'KARE_CIZ' yazarsak bilgisayar bu islemleri yapsin (Metot mantigi):
TO KARE_CIZ
    
    ; FOR / VEYA REPEAT DONGUSU: 
    ; Icindeki Koseli Parantezi [ ] TAM OLARAK 4 Defa Tekrarla (Cunku Kale 4 Kenarlidir)!
    REPEAT 4 [
        FORWARD 100   ; 100 Adim Ileri Git (Çizgiyi Çek)
        RIGHT 90      ; Oldugun Noktadan 90 Derece Saga Don!
    ]

END ; Proseduru beynine kaydet!


; 2. DEVASA BİR YAPRAK/ÇİÇEK MOTİFİ ÇİZMEK İÇİN FONKSİYONU ÇAĞIR:
; Sadece Cizgilerle Bir Cicek Sanati yapalim (İçi İçe Döngü Mucizesi):

REPEAT 36 [          ; 36 Kere Çiçek Yaprağı döngüsü
    KARE_CIZ         ; Yukarida icat ettigimiz Kareciz Fonksiyonunu Cagir! (Bir kare cizecek)
    RIGHT 10         ; Bir kare bittikten sonra kaplumbağa 10 derece sağa donsun ki cicek patlasın!
]

; Ekrana Dönen ve İciçe Geçmiş 36 Tane kareden oluşan devasa ve mükemmel bir Mandala(Cicek) cizildi!
```
Eskiden ilkokul çocukları Commodore 64 veya MS-DOS başındayken bu kodla devasa sanat eserleri ürettiler. "Açı" hesaplamasını ezberlemek/köklemek yerine bir nevi oyun felsefesine dahil oldular.

## Kimler Kullanır?
* **1980 ve 90'ların Bilgisayar İlk Öğrenicileri (Artık hepsi yaşını almış ustalar)**. Evinde ilk defa Commodore veya Apple II bilgisayarla tanışanlar için "Ekrana Hello World" basmaktan çok daha zevkli olan ilk dillerinden biriydi.
* Günümüzde doğrudan "Logo" dili (eski siyah terminal haliyle) kullanılmaz, çünkü **Scratch** onun tahtını ve ruhunu tamamen geliştirilmiş Blok mantığıyla almıştır. Ancak Lisp bazlı bu kaplumbağa mantığı (Turtle Graphics), her üniversitede CS (Bilgisayar Birimi) öğrencilerine giriş kursunda "Algoritmik geometriyi" kavratmak adına (Python'a Turtle eklentisi takılarak vb) yaşatılmaya devam eder.
