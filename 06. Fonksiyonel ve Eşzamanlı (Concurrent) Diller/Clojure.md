# Clojure

## Özet
Clojure; 2007 yılında Rich Hickey tarafından geliştirilen, eski (1950'ler) Lisp dilinin parantezlerle dolu saf fonksiyonel felsefesini, inanılmaz popüler Java Sanal Makinesi'nin (JVM) muazzam gücü ve modern kütüphaneleri ile birleştirerek efsaneleştirdiği, veriyi "veri olarak" yöneten (Code is Data) hibrid bir "Eşzamanlı Fonksiyonel" programlama dilidir.

## Nedir ve Ne İşe Yarar?
1950'lerden kalma Lisp dili öylesine güçlüydü ki; kodları ve veriyi aynı parantez biçiminde (`( )`) tutardı. Bu ona program çalışırken kendi kendini değiştirebilen Yapay Zeka (AI) yetenekleri (Metaprogramlama) veriyordu. Fakat Lisp, "Gerçek hayattan (Java kütüphanelerinden, Windows dosyalarından)" çok uzak kapalı kutu akademik bir ortamdı.

Rich Hickey, Lisp'i aldı ve doğrudan **Java'nın beyninin (JVM)** içine monte etti. Bu devrim niteliğindeydi çünkü Clojure yazarken, ihtiyaç anında Java'nın milyarlarca saatlik efsanevi o kütüphanelerine (Örn: Veritabanı bağlama, PDF üretme, Şifreleme) bir saniyede (tek bir nokta " `.` " koyarak) erişip çağırabiliyordunuz.

**Ne İşe Yarar?**
* **Bulut / Finans Veri Bilimi (Big Data):** Devasa veri denizlerinde (Örneğin Wal-Mart'ın saniyelik alışveriş istatistiklerinde) eşzamanlı ve değişmez (Immutable) işlemleri hatasız paralel çalıştırır.
* **Canlı Kodlama (REPL Driven Development):** Yazılım geliştirmeyi tamamen kökünden değiştirir. Yazılımcı kodu yazar, 'Derle (Compile) ve Bekle' yapmaz. Arka planda sunucu (Örn. REPL) zaten çalışıyordur, siz yazdığınız an kod saniyeler içinde çalışan sisteme zerk edilir. Mühendisler projeyi *uçarken inşa* ederler.

## Dilin Mantığı ve Kod Yapısı
Sözdizimi klasik yazılımcıları ilk başta çıldırtan bir **"Parantezler Cehennemi (Homoiconicity)"** olarak bilinir. `(((())))` şeklinde iç içe geçen parantezler Lisp dil ailesinin kalbidir.

Clojure'da operatör (örnek: toplama `+`) başa yazılır (Prefix Notation). Matematikte `2 + 3` yazarsınız (İnsan dili). Lisp'te ise fonksiyonel emri önce verirsiniz: `(+ 2 3)`. Sadece toplama değil, fonksiyon dairesi, dizi, koşullar her şey bu "Liste Kavrayışı" na hapsolmuştur.

Erlang ve Elixir gibi, Clojure da verilerin **Değiştirilemezliğine (Immutability)** yeminlidir. Java Sanal Makinesi'nin o "Aman Çöp Toplayıcıları (Garbage Collector) dikiş tutsun, Thread'ler (İkili iş dizileri) kitlenmesin" sorununu Fonksiyonel kökeniyle %100 Paralelleştirerek aşar (STM - Software Transactional Memory hilesi).

**Örnek İşleyiş (Sembolik Olarak):**
Liste mi var? Bir Liste yaratıyoruz: `(1 2 3)`. Listenin başına fonksiyon mu (topla) koyduk? `(+ 1 2 3)`. Dinamik mi yapıyoruz? `(defun ..)`. Bu efsanevi sadelik, Kodun Kendisinin Veriyle Aynı Görünmesi demektir (Buna *Kod Veridir, Veri Koddur - Code as Data* denir). Clojure kendini rahatça değiştiren, kendini güncelleyen makrolar üretebilir.

### Örnek Bir Clojure Kodu: Sembolik Lisp Sözdizimi ve Java Entegrasyonu
Klasik `for` döngülerinin olmadığı, verinin sadece matematiksel `map/filter` akışıyla çekildiği ve JVM (Java) güçlerinin çağrıldığı o ünlü Lisp (Parantez) mimarisi:

```clojure
;; Clojure'da yorum satırları (noktalı virgül) ile başlar 

;; Java'nin kütüphanesini cagiriyoruz! JVM'in o gücünü bedavaya aliyoruz.
(import java.util.Date)

;; Bir fonksiyon/iş yapıcı Tanimlayalim: (defn fonksiyonAdi [parametre] (ne_yapılacağı) )
(defn kisiyi-selamla [isim]
  
  ;; Java kütüphanesini çağırarak Java objesini yaratıyoruz (Date.)
  (let [su-an (Date.)]  ;; Java objesini Clojure degiskenine hapsettik (Tarihi)
    
    ;; Metin birleştirme operatörü "str". (Parantez icindeki her sey str parametresidir)
    (str "Merhaba Sayin " isim "! Sistem Saati Java uzerinden: " su-an)))


;; FONKSİYONU ÇAĞIRALIM: (Operator / Fonksiyon EN BAŞA koyulur!)
(println (kisiyi-selamla "Dev"))
;; Çıktısı: Merhaba Sayin Dev! Sistem Saati Java uzerinden: Tue Feb 22 17:35:00...


;; ============================================
;; SAF FONKSIYONEL GUC (Listeleri Eşzamanlı İşleme):

;; Diyelim ki elimizde milyonlarca sayı listesi var ve ciftleri ayiklayip karesini alacagiz:
;; "def" : Degismez (Immutable) bir kume tanimlari
(def sayilar [1 2 3 4 5 6 7 8 9 10])

;; Clojure'un Muhtesem 'Boru/Zincirleme Yöneltme (Thread-last)' Makrosu `->>`:
;; Sayilari al, once ÇİFT mü diye filtrele, sonra Karesi ile Çarp (Haritala/Map)

(def sonuclar 
    (->> sayilar                  ;; Listenin Ana verisi
         (filter even?)           ;; SADECE Çift olanlari süz, alta at
         (map #(* % %))))         ;; Kalanlarin (% kendi degerleri demek) kendisiyle ÇARP (Karesi)

(println "Sonuc Listemiz (Lisp Gucuyle) :" sonuclar)
;; Çıktısı: Sonuc Listemiz: (4 16 36 64 100)
```

Bu lisp parantezli kod yapısı, arkada JVM sayesinde paralel şekilde patlayıp veriyi hatasız ve lag olmadan ayıklar. Bütün finans sistemi veya analitik buna dayanır.

## Kimler Kullanır?
* Finans Analistleri, Borsa (Trading Sistemi) Mimarisi programcıları ve "Veri Tarlalarında" (Wal-Mart Lojistiği, AWS iç operasyonları) dolaşan ve verinin çökmeyip aynı anda paralel analiz edilmesi gereken yapılar arayan analitik/algoritma profesyonelleri.
* "Ben REPL ile kodlarken sistem zaten açık olsun, durup baştan proje başlatma/derleme zamanı kaybıyla uğraşamam, anında sistem tepkimelerini görmeliyim" arayışında olan elit ClojureScript Start-upları (Örn: CircleCI, Nubank veya Puppet vb.)
* Yapay zeka ve Veri Manipülasyonlarında kodun kendisini değiştirmek için (Meta-programlama) devasa "Makro" (Lisp Mirası) yeteneğinden faydalanan araştırmacı mühendisler.
