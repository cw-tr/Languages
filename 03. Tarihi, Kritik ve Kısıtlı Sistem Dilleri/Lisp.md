# Lisp (LISt Processing)

## Özet
Lisp; 1958 yılında (Fortran'dan hemen sonra) M.I.T'den John McCarthy tarafından yaratılmış olan, dünyanın günümüze kadar kullanımda kalabilmiş en eski ikinci programlama dili, **Evrendeki "Yapay Zeka (AI)"nın Ana Babası**, Veriyi ve Kodu aynı yapıda (Parantezlerle) kabul eden ve Fonksiyonel Programlamanın kurucusu olan destansı, büyücü bir teknoloji mucizesidir.

## Nedir ve Ne İşe Yarar?
1958 yılında bilgisayarlar makine dili, 0'lar, 1'ler ve Matris çarpmalarından ibaretti. John McCarthy bir kağıda tamamen matematik felsefesinden doğan Lambda Kalkülüsünü (Alonzo Church'ün kuramı) çizerken, o güne kadar icat edilmemiş çılgın bir yapı buldu: **Tüm Programlama dili Mantiğini SADECE Liste `( , )` işleme üzerine kodlayabilirim!**

Makale yayınlandı (eval/apply mekanizmaları makalesi), McCarthy'nin öğrencisi (Steve Russell) hocasının makalesindeki matematiksel formülleri aldı ve derleyicisini IBM 704 bilgisayarı üzerinde "Tüh, bu gerçekten donanımda çalışabiliyormuş!" diyerek hayata geçirdi. O gün Sembolik Yapay Zeka (AI) dünyası patavatsızca doğdu.

**Ne İşe Yarar?**
* **Tarihi Kökenli Yapay Zeka (AI) Algoritmaları:** Robotik planlama sistemleri, Satranç analizörleri ve İnsan gramerini çözen ağlar, Lisp'in listeleri tarayan ve KENDİ KENDİNE YENİ KOD oluşturabilen (Metaprogramlama / Macros) mimarisiyle yazıldı. Emacs Editörü Lisp ile çalışır (Emacs Lisp).
* **Uzay Programları ve Sembolik Denklemler:** NASA'nın 1990'lardaki devasa uzay denetimcileri (Remote Agent) Lisp kodu atıyordu. Hatta Lisp öylesine büyüseldi ki sistem uzayda hata verdiğinde, NASA mühendisleri "Dünyadan Uzay Aracına Canlı Debugging (Müdahale)" yaparak program yayındayken kilitleri aşmışlardır.

## Dilin Mantığı ve Kod Yapısı
Evrende gördüğü en önemli ve yegâne şey **PARANTEZDİR `(...)`** ve Lists (Listelerdir). Java'daki For, C'deki If, Assembly'deki Goto.. Hepsi eziyettir. Lisp (Kendisinin modern versiyonlarından biri olan Common Lisp); her bir bloğu Parantezlerle böler.

Sözdizimi "Homoiconic (Eşyapısal)" dır. Yani **Kod aynı zamanda Veridir! (Code is Data)**. Sizin yazdığınız komut dosyası aslında bir Listedir, ve Lisp listeleri harf harf gezerek işleyebildiği için, program kendi kendinin kaynak koduna bakıp onu anında değiştirerek yep yeni bir işleme devam edebilir (Makro Makinesi). Bu, Yapay Zekânın ana fikir omurgasıdır.

Lisp Klasik matematikteki "İnfix `(2 + 2)`" kullanmaz. "Prefix - Öne koyan gösterimi `(+ 2 2)`" kullanır.

**Örnek İşleyiş (Sembolik Olarak):**
Python'da siz bir dizi listesi yapıp tersine çevirip ve en büyük sayıyı döndürmek için döngüler yazarsınız.
Lisp: `(max (reverse (1 2 3 4)))`. İşlev, içteki parçaya dalıp oradan dışarı doğru balon gibi genleşir. Matematikteki tam fonksiyonel bir ifadedir $(f(g(x)))$.

### Örnek Bir Lisp (Common Lisp) Kodu: Parantez Şaheseri (Fonksiyon, Öz Yineleme ve Listeler)
Geleneksel hiçbir döngünün yer alamadığı (Loop/While bulunmaz), her şeyin Parantezin en başına "Fonksiyon İsimi fırlatarak" Recursive (Kendini Çağırma) mantığıyla yürüdüğü klasik bir Liste İşleme Kod Dizilimi:

```lisp
;; Lisp Dilinde yorumlar (Noktali virgul) ';' ile başlar.
;; (Kalin Parantez Okyanusu dedikleri efsaneye hosgeldiniz).

;; 1. İLK ATAMA VE EKRANA YAZI (DEF PARAMETER - DEFINITION)
;; 'selamla' isminde bir Mesaj sabiti (Metin) Tanimliyoruz
(defparameter *mesaj* "Lisp'in Sembolik ve Yapay Dünyasina Hosgeldin!")

;; Ekrana Formatli (Format ' t) Basma Fonksiyonu. Fonksiyon en başa gider!
(format t "~a~%" *mesaj*)


;; 2. RECURSIVE (KENDİNİ YİNELEYEN) YAPAY ZEKAA KOKAN FONK.(Defun):
;; Lisp'in gucu For döngüsünde degildir, Bir dizideki Listenin uzunlugunu
;; matematiksel bir şelale gibi akan "Cagirici" Lisp (Ozyineleme) fonskiyonudur: 

(defun listemin-uzunlugu (gelen_liste)
  
  ;; Eger (if) gonderilen liste boşsa (null) , 0 Rakamı Dön
  (if (null gelen_liste)
      0
      
      ;; EGER BOS DEGILSE, DEVAM EDİYORUZ:!
      ;; LISP'IN IKI MUCİZE KELİMESİ: CAR(ilk veri) ve CDR(kuyruktaki kalan veri).
      ;; "Bana 1 Ekle (+ 1) -> Ve Kuyruktaki kalanı tekrar ayni foksiyona ListeminUzunluguna yolla"
      
      (+ 1 (listemin-uzunlugu (cdr gelen_liste))))) 


;; === SİSTEMİN İŞLETİLMESİ (Fonksiyonu Çagırma ve Listeler) ===

;; Bir LİSTE Verisi Yaratiyoruz (İçine elma objesi de, Sayi da konabilir, Dinamiktir):
;; Tirnak Isareti '()  (Quote) lisp'in listeyi calıstırma degil sadece VERI olarak saklamasini soyler
(defvar cantam '(kılıc kalkan iksir 100_altin binek_at))

;; Matematik Fonksiyonunda En başa kendi fonksiyonumuzu ve icine kargoyu verip Konsola dök(print):
(print "Çantamdaki Nesne Sayısı Analizi Başlıyor..")
(print (listemin-uzunlugu cantam))

;; Ciktisi : 5
```

Ve Programcı Lisp makrosunu çağırıp o kodların içine kendi yazdığı `listemin-uzunlugu` adlı fonksiyonun da başka bir "Liste (`defun` ile başlayan)" olduğunu görür ve Programı çalışırken programın yapısını değiştiren kod yazarak felsefenin zirvesine ulaşır.

## Kimler Kullanır?
* 1960, 70 ve 80'li yıllarda dünyadaki %100 oranında bütün Profesörler, Yapay Zeka Laboratuarları, Mantık/Dil çözümleyici Enstitü Üniversite projeleri ve "Lisp Makineleri (Özel Lisp ile çalışan bilgisayarlar) Askeri İstihbarat kodlayıcıları". 
* Modern günümüzde ise ruhu (Dialect'i) olan **Clojure (JVM Üzerinde koşan)** vasıtasıyla hala modern finans ve devasa bulut algoritmalarında, veri-bilimi/manipülasyonu kullanan dev firmalar ve "Lisp Makroları olmadan yaşayamam" diyen çok zeki/elit (Paul Graham vb.) usta Backend Hacker ekipleri. 
* Kredi onayı/banka algoritmalarında hala "Common Lisp" veya "Scheme" ile direnen çok nadir efsaneler yaşar.
