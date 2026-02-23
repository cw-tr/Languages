# Racket

## Özet
Racket (Eski adıyla PLT Scheme); 1995 yılında geliştirilen, temelinde Lisp ve Scheme'in zarif felsefesini barındıran ancak sadece bir programlama dili olmaktan çıkıp **"Yeni Programlama Dilleri Yaratmak (Language-Oriented Programming)"** için icat edilmiş efsanevi, kapsamlı ve akademinin de ötesine geçmiş modern bir fonksyonel ekosistemdir.

## Nedir ve Ne İşe Yarar?
Scheme dilini öğrenen akademisyenler "Bu dil harika ama kütüphaneler (Web/Dosya) eksik ve sadece teorik kalıyor" diyorlardı. Ayrıca birileri web sitesi yapmak, diğeri matematik teorisi ispatlamak istiyordu.

Racket yaratıcıları dedi ki; "Biz size bir dil vermeyeceğiz, biz size **DİL FABRİKASI (Ecosystem)** vereceğiz". Racket içinde muazzam Makro sistemleri (Syntax-rules/extend) barındırır. Eğer "Racket'in C diline benzemesini" ya da "Sadece SQL gibi davranan yepyeni bir DİL olmasını" istiyorsanız; Racket size *10-15 satır makro koduyla Kendi Dilinizi Yaratma (Domain Specific Languages - DSLs)* gücünü sunar. DrRacket adında harika, eğitimcilere özel kendi IDE'siyle birlikte gelir.

**Ne İşe Yarar?**
* **Dil-Yönelimli Programlama (Language-Oriented):** Racket ile yazılım projesi kodlanmaz; "Bu projeyi kodlayacak Özel Dil" icat edilir, sonra o dil ile proje çözülür. Yeni diller inşa etmek oyun hamuru oynamak gibidir.
* **Akademi ve Sembolik Yapay Zeka:** Scheme'in mirasını devam ettiren Amerikan üniversitelerinin (Örn: Northeastern University) Bilgisayar dilleri teorisi üretme merkezidir.
* Güçlü Web Kütüphaneleri ile "Pratik/Endüstriyel Lisp" açığını kapatır. (Hem betik, hem Web sunucusu koşturabilirsiniz).

## Dilin Mantığı ve Kod Yapısı
Tam bir Scheme türevidir lakin dosyanın başına MUCİZE BİR HASTAG ATILIR: **`#lang racket`**.

İşte bu komut derleyiciye şunu der: "Benim altımdaki bütün kodları, 'racket' isimli dilin kurallarıyla derle/oku".
Eğer dosyanın başına `#lang typed/racket` yazarsanız, Racket bir anda C++ gibi *Katı Tipli (Strict)* bir dile dönüşür!
Eğer dosyanın başına `#lang datalog` yazarsanız, Racket saniyesinde Yapay Zeka Logic/Prolog tarzı mantık kuralları diline dönüşür. Rüzgar gülleri gibi saniyede dil değiştirirsiniz.

**Örnek İşleyiş (Sembolik Olarak):**
Diğer lisp'lerden tek farkı sadece Parantez değil ( `(...)` ), okumayı kolaylaştırmak için köşeli parantezi `[...]` de listelemelere izin vermesidir (Semantik olarak aynı çalışır ama göz yormaz).

### Örnek Bir Racket Kodu: Yeni Bir "Dil(Sentaks) Parçacığı" İcat Etmek!
Racket'in en büyük büyücülüğü Makro (Define-Syntax) ustası olmasıdır. Sırf canınız C#/Java'daki "While" döngüsünün aynısını çekiyor diye, Lisp'e *1 Saniyede For/While Döngüsü ÖĞRETME* sanatı:

```racket
#lang racket 
;; ^ BU İŞARET ('#lang racket') BU DOSYANIN RACKET ANA DİLİNDE DERLENECEGİNİ SOYLER ^


;;  === YENI DİL MİMARİSİ İCAT ETMEK (MACROS - SYNTAX-RULES) ===

;; Birileri Lisp'te "For Cok Zor (For-each fln)" diyor degil mi?
;; Hadi Racket'te WHILE diye yep yeni bir kelime uretelim (Derleyiciye Hack):

(define-syntax-rule (while sarti_kontrol_et kod_govdesi ...) 
  ;; Bu while gördugun an onu "rekürsif (kendi kendini cagiran) loopt-dongusune" cevir!
  (let loop ()
    (when sarti_kontrol_et
      kod_govdesi ...
      (loop))))

;; VE TELAŞA GEREK YOK, ARTIK C DİLİ GİBİ BİR DİLİNİZ VAR!! (Metaprogramlama)


;; === İŞLETİM BÖLÜMÜ ===

;; C Dilindeki Atama (Sayaci 0'dan baslat)
(define sayac 0)

;; Bizim Icat ETTİĞİMİZ C++ tarzi "while" kelimesini calistiralim!
(while (< sayac 5) 
  
  (display "Racket ile Sahte Döngü Dongu Donuyor:")
  (display sayac)
  (newline)
  
  ;; (set!) komutu Racket'de Değiskenin ICINI MUTASYONA UGRATIR / Degistirir (Impure eylem)
  (set! sayac (+ sayac 1)))
```
Siz kodu çalıştırdığınız an; Racket o "while" kelimesini alır, arkaplanda binlerce Lisp fonksiyonuna/recursion loopuna bağlayarak felsefi eksiği kod satırı eylemine dönüştürür.

## Kimler Kullanır?
* "Kendi dilimi tasarlamalıyım!" diyen bilgisayar dili mühendisi ve Mimarları.
* Amerika'daki okullarda Scheme yerine artık Lisp eğitimi vermek isteyen Lise ve Üniversite hocaları. ( *How to Design Programs* - HTDP adlı ünlü bilgisayar ders kitabı baştan aşağı DrRacket formatında yazılmıştır.)
* Video oyunlarına veya Web sayfalarına "Statik/Typed" fonksiyonellik katmak isteyip, Makrolar olmadan yaşayamayan hardcore "Eski Toprak" Lisp-sevici bilgisayar hackerları. Kurumsal şirketler Racket'i doğrudan üretimde kullanmazlar lakin kendi iç dil (DSL) sistemlerini yazarken altyapısını soyarlarlar.
