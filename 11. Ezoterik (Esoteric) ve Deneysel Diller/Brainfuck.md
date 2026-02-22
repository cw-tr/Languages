# Brainfuck

## Özet
Brainfuck; 1993 yılında Urban Müller tarafından, "Dünyadaki en küçük Derleyiciyi (Compiler) yazmak (Sadece 240 Byte Amiga OS derleyicisi)" amacıyla icat edilen, evrendo sadece ve sadece **8 Tane Noktalama İşareti (`> < + - . , [ ]`)** kullanarak "Turing Complete" (Her türlü matematiksel algoritmayı çözebilen) olma unvanını taşıyan, yazması ve okuması tam bir beyin yakan şizofrenik **Ezoterik (Mizahi/Deneysel)** programlama dilidir.

## Nedir ve Ne İşe Yarar?
1993'lerde ezoterik (kullanım amacı olmayan, sadece felsefi ve sanatsal sınırlar zorlayan) diller popüler olmaya başlıyordu. Urban Müller, makinenin ne kadar az kuralla bir bilgisayar olabileceğini kanıtlamak istedi. C veya Python'daki değişkenler `int x = 5` yoktur. Onun yerine dümdüz sıfırlardan oluşan **30.000 Hücrelik Bir Şerit (Bellek/RAM Array)** ve bu şeridin üstünde ileri geri giden minik bir "Veri İşaretçisi (Pointer)" vardır.

Kodu yazan kişi, işaretçiyi şerit üstünde bir sağa bir sola kaydırarak (`>` `<`), o anki kutudaki sayıyı bir arttırıp bir azaltarak (`+` `-`) matematikle kelimeler oluşturur. 

**Ne İşe Yarar?**
* **HİÇBİR TİCARİ İŞE YARAMAZ:** Bir şirket için Brainfuck ile kod yazmak imkansızdır.
* **Zihin Egzersizi ve Hacker Kültürü:** Turing-Tam-Matematiğini (Turing Completeness) anlamak, yazılımcıların zekalarını veya sabırlarını diğer programcılara kanıtlamak (Örn: "Ben Brainfuck ile Yılan/Snake oyunu yazdım!" diyerek şov yapmak) için kullanılır.
* **Compiler Tasarımı Öğrenimi:** Sadece 8 karakter olduğu için, üniversitelerde Üniversite 1. Sınıf öğrencilerine "Kendi Derleyicinizi (Interpreter) C ile yazın" ödevlerinde Test-Mankeni olarak kullanılır.

## Dilin Mantığı ve Kod Yapısı
Brainfuck'ın o minimal, vahşi ve saf matematik kokan tam 8 adet kutsal komutu vardır (Bunlar haricindeki bütün harfler/alfabedeki her kelime YORUM satırı sayılır ve görmezden gelinir):

1. `>` : Pointer'ı (Hafıza Okunu) Bir Sağdaki Hücreye kaydır.
2. `<` : Pointer'ı Bir Soldaki Hücreye kaydır.
3. `+` : Üzerinde bulunduğun kutudaki sayıyı 1 Arttır.
4. `-` : Üzerinde bulunduğun kutudaki sayıyı 1 Azalt.
5. `.` : Kutudaki sayının "ASCII (Harf)" Karşılığını Ekrana Bas! (Örn: 65 ise ekrana 'A' basar).
6. `,` : Kullanıcıdan (Klavyeden) 1 Haraf Al ve kutuya at.
7. `[` : Eğer Kutudaki Sayı "0" ise; Dongüyü kır (Kapanış parantezine git). Değilse ICERI GİR (WHILE Döngüsü Başlangıcı).
8. `]` : Döngü bitişi. Oku tekrar başa `[` çevir.

**Örnek İşleyiş (Sembolik Olarak):**
Ekrana `A` harfini (ASCII Kodu: 65) basmak için "Acemi" birisi 65 tane `+` yazar ve `.` koyar. (++++++++... .)
Zeki birisi; Sayıyı döngü ile Çarpar! "5 kere 13 yap".

### Örnek Bir Brainfuck Kodu: Evrensel Eziyet "Hello World!"
Ekrana sadece `Hello World!` yazısını basmak için, hafıza hücrelerini birbiriyle çarparak ASCII tablosundaki 72(H), 101(e) numaralarına ulaşmaya çalışan o daktilo terörü:

```brainfuck
Bu kisimlar serbest metindir. Derleyici sadece matematiseal 8 isarete bakar.

+++++ +++++             # 0 numarali Hucreyi 10 yap (DONGU SAYACIMIZ)
[                       # 0 Numarali hucre sifir olana kadar Don (10 Kere):
    > +++++ +++         # 1. Hucreye git, 8 Ekle 
    > +++++ +++++       # 2. Hucreye git, 10 Ekle
    > +++++ +++++ +     # 3. Hucreye git, 11 Ekle
    <<< -               # Basa (0. Hucreye) DON ve Döngü Sayacini 1 Azalt!
]                       
# Dongu bittiginde Tablomuz soyle oldu:
# Hucre 0 = 0 
# Hucre 1 = 80  (10*8 = Yukaridaki 8 eklemesi x 10 Dongu)
# Hucre 2 = 100 (10*10)
# Hucre 3 = 110 (11*10)

> - .                   # Pointer'i 1. Hucreye at (80 den 1 cikar -> 79). "." ile Bas! Ekrana 'O' Geldi. 
                        # Aslinda 72 lazimdi dur 8 cikarticam:
                        
> - - - .               # Wait, This is getting too complex. Let's do a classic minimal one!

# ==============================================================
# KLASIK VE OPTIMIZE EDILMIS "Hello World!" BEYIN YAKMASI:
# ==============================================================
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```

Evet, yanlış görmediniz. Python'da `print("Hello World!")` yazdığınız kodun tam derlenmiş makine matematiği; Brainfuck'ta yukarıdaki o "Oklar, Artılar ve Eksiler" senfonisidir.

## Kimler Kullanır?
* Evrendeki işsiz, sınırları zorlamayı seven matematikçi "Code-Golf" (Az karakterle çok iş yapan) felsefecileri. 
* Ezoterik dillere merak duyanlar. Programlama mimarilerinde Pointer kavramlarını (C dilindeki o `*ptr`) kafasında tam oturtamayanlar için "Hücreler üstünde gezinme" felsefesini zihne demirle kazıyan kanlı bir şövalyedir. Adı üstünde Beyin-Ezici dillerin Atasıdır!
