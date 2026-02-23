# Scheme

## Özet
Scheme; 1975 yılında M.I.T'den Guy L. Steele ve Gerald Jay Sussman tarafından yaratılan, orijinal "Lisp" dilinin inanılmaz karmaşık ve büyük yapısını **temizleyip en minimal, en pürüzsüz ve en zarif** haline getiren, yazılım eğitiminde (metaprogramlama ve derleyici mantığında) evrendeki en kült fonksiyonel (Dialect) dillerden biridir.

## Nedir ve Ne İşe Yarar?
Orjinal Lisp (Common Lisp) 1970'lerde o kadar çok özellik ile şişmişti ki (Aynı isme sahip hem fonksiyon hem değişken olabiliyordu, makrolar karmaşıktı), yeni programlamaya başlayan öğrenciler Lisp makinalarında boğuluyordu. 

Scheme, "Dili 50 sayfalık bir sözdizimi kitabına indirelim" diyerek çıktı. Sadece 5-10 tane ana (ilkel/primitive) kural barındıran muazzam temiz bir çekirdek Lisp'tir. En büyük devrimi ise **Tail-Call Optimization (Kuyruk Çağrısı Optimizasyonu)** kuralını evrene ilk getiren ve standartlaştıran dil olmasıdır. (Yani kendi kendini çağıran/Recursive bir fonksiyon, For döngüsü kadar RAM'de az yer kaplasın diye hafızayı otomatik temizler).

**Ne İşe Yarar?**
* **M.I.T Üniversite Eğitimi:** Evrendeki en meşhur programlama kitabı sayılan **"SICP (Structure and Interpretation of Computer Programs)"** (Sihirbaz Kitabı) M.I.T'de 20 yıldan fazla bir süre boyunca baştan ayağa "Scheme" dili kullanılarak yazılım mühendislerine kodu değil, *yazılım felsefesini* eğitmek için kullanılmıştır.
* **Domain Specific Language (DSL):** Photoshop'un eski eklentileri, AutoCAD algoritmaları veya Müzik Kütüphanelerinin içine "Gömerek (Embed)" küçük betikler çalıştırmak isteyen sistem programcıları C'nin yanına Scheme gömer.

## Dilin Mantığı ve Kod Yapısı
Tamamen (Parantezli) Lisp'dir. Ancak Lisp-1 (Değişken ve Fonksiyonların isim alanları ORTAK/Single Namespace) yapısına sahiptir. Yani bir şey isimse, aynı anda fonksiyon isimiyle çakışamaz.

Sürekli (Recursion / Öz-Yineleme) üzerine dayalıdır. Değişken atamaları (State mutation) yapmaktan nefret eder ve kaçınır.

**Örnek İşleyiş (Sembolik Olarak):**
`define` kelimesi Scheme'nin omurgasıdır.
Değişken tanımlama: `(define x 10)`
Fonksiyon tanımlama: `(define (topla a b) (+ a b))` 
Kondisyon(if): `(if (> x 5) "Buyuk" "Kucuk")`

### Örnek Bir Scheme Kodu: Efsanevi Felsefe (SICP Tail-Call Optimizasyonu)
Sihirbaz Kitabından kopup gelmiş, C dilindeki çirkin "While/For" döngüleri yerine, "Ben kendimi sonsuza kadar çağırayım ama Tail-Recursion sayesinde JVM(veya RAM) asla patlamasın (Stack-Overflow olmasın)" mucizesi (Faktöriyel):

```scheme
;; Scheme (ve tum Lisplerde) Yorumlar Noktali Virgul (;) 

;; DIKKAT: CL (Common Lisp) 'defun' kullanirken, Zarif Scheme SADECE 'define' kullanir!
;; (faktoryel-hesapla n) isminde bir Fonksiyon deklare et!
(define (faktoryel-hesapla n)

  ;; IC-FONKSIYON (Helper): Evrenin ilk Tail-Call (Kuyruk) Algoritmasi:
  ;; Disariya hicbir degisken (var) sizdirmadan İÇERİDEKİ argumanlari (sayac ve sonuc) ceviriyoruz!
  (define (dongu-isçisi sayac anlik_sonuc)
    
    (if (> sayac n) ;; EGER Sayac N'i astiysa; Islem bitmistir.
        
        anlik_sonuc ;; True: Sonucu patlat ve Geri don (Return)
        
        ;; FALSE ise: FOKSIYONU KENDİ İÇİNDE BİR DAHA ÇAGIR (Recursion Mutasyonu!)
        ;; (Sayaci 1 artir, Sonucu sayacla carpip Parametreyi GÜNCELLE!)
        (dongu-isçisi (+ sayac 1) 
                      (* anlik_sonuc sayac))))
  
  ;; Ana fonksiyon helper'i ilk parametrelerle (sayac 1, sonuc 1) Atesler:
  (dongu-isçisi 1 1))

;; Ekrana (Console / REPL) Bas:
(display "5 Faktoriyel'in Sonucu: ")
(display (faktoryel-hesapla 5))
(newline)

;; Cikincida Konsolda: "5 Faktoriyel'in Sonucu: 120"
```
Bu koddaki `(dongu-isçisi)` mekanizması C#'da on bin kere yapılsa Hafıza Patlaması/Memory Exception verir. Lakin Scheme bunu "Döngü Optimizasyonuna (Goto gibi)" dönüştürüp hiç RAM doldurmadan çalıştıracak o efsanevi Standart Akademik kurala bağlıdır.

## Kimler Kullanır?
* Evrensel Bilgisayar Mimarisi, Derleyici (Compiler) Mantığı ve "Kendi dilimi kendim nasıl yazarım" teorisini felsefi olarak öğrenen Akademi/Bilgisayar Bilimleri (Computer Science) Profesör ve Öğrencileri.
* Guile, Racket gibi kendi Scheme şiveleriyle Linux ortamlarında Makro konfigürasyonu (Örn: GNU Guix Paketi yönetimi) yazan Stallman / Özgür Yazılım vakfı felsefecileri. 
* Modern Endüstride asla web/mobil yapmak için kullanılmaz, **"Kodu düşünmeyi öğrenme"** felsefesinin saf Budizm tapınağıdır.
