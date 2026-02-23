# CoffeeScript

## Özet
CoffeeScript; 2009 yılında Jeremy Ashkenas (ayrıca Backbone.js'in yaratıcısı) tarafından icat edilen, 2010'lu yılların başında JavaScript'in (ES5 dönemi) o süslü parantezli, uzun ve çirkin (`function() { }`) yapısından nefret eden geliştiriciler için **"JavaScript'in daha kısa okunur hali (Syntactic Sugar - Sözdizimi Şekeri)"** olarak kodlanıp, arkaplanda anında Pürüzsüz JavaScript'e çevrilen (Transpile olan) ve JS evrimine ilham kaynağı olmuş efsanevi bir proxy/betik dilidir.

## Nedir ve Ne İşe Yarar?
2010 yıllarında Web tarayıcılarında çalışan tek dil **JavaScript**'ti. Ama JS o dönem çok eski kurallarda kalmıştı; Sınıf (Class) yapısı yoktu `prototype` gibi garip şeyler yazılırdı, kısa ok işareti (`=>`) yoktu uzun uzun `function` yazılması gerekiyordu. 

Jeremy Aşkenas dedi ki: "Neden Python ve Ruby gibi Noktalı Virgülsüz(;), Süslü Parantezsiz `{}`, boşluk (indentation) ile hizalanan harika bir dil icat etmiyoruz? Geliştirici bu kodla `script.coffee` dosyasını yazar, bizim derleyicimiz saniyede O kodu alır, içine süslüleri/return'leri ekler ve Tarayıcının anlayacağı saf `script.js` koduna çevirip fırlatır!" Böylece CoffeeScript dünyayı kasıp kavurdu. (Hatta Dropbox ve GitHub bir dönem arka planlarını tamamen CoffeeScript'e geçirmişti!).

**Ne İşe Yarar?**
* **Hızlı JavaScript Üretimi (Eski Front-End):** Web sitelerindeki açılır menüler, Ajax kontrolleri ve UI animasyonları; JS yazıp tırnak içine girmektense CoffeeScript ile daktilo yazar gibi 3 kelimede birleştirilirdi. Geliştirici sayfasında Coffee yazardı, NodeJS/Gulp o sayfayı anında JS'e (Minified) çevirip Chrome'a yollardı.
* **Modern JavaScript'in "Öz" Atasıdır:** Bugün 2020'lerde kullandığımız Modern JavaScript'teki (ES6) Sınıflar `class`, Arrow Fonksiyonlar `=>` ve String Birleştirme `${}` özellikleri aslında **Okyanusun karşısındaki CoffeeScript'ten özenilerek/kopyalanarak** Resmi JS'ye (ECMAScript'e) aktarılmıştır. JS'ye yön veren bir fener olmuştur.

## Dilin Mantığı ve Kod Yapısı
Tam bir **Python** ve **Ruby** kırmasıdır. Sınırlanmış (Scoping) bloklar için Süslü Parantez ({}) kullanılmaz; Python gibi **Boşluklara (Indentation/Tab)** bakılır. Komutların sonuna eklenen Noktalı virgüller (;) kaldırılmıştır. 

En büyük şovunu Fonksiyonlarda yapar. `function` kelimesi silinmiş, yerine **Kısa Ok İşareti `->`** icat edilmiştir! 
Tüm fonksiyonlar otomatik olarak (Ruby gibi) son satırındaki değeri Geri Döndürür (Implicit Return), o yüzden `return` yazmaktan da kurtarır!

**Örnek İşleyiş (Sembolik Olarak):**
Eski JS: `var kare_hesapla = function(x) { return x * x; };`
CoffeeScript Mucizesi: `kare_hesapla = (x) -> x * x` (Bu kadar! Arkaplanda kendisi function ekler).

### Örnek Bir CoffeeScript Kodu: "Function" ve "Prototype" Eziyetini Çöpe Atmak
O dönemin en kanser JavaScript eylemlerini; Sanki Ruby veya Python yazıyormuş gibi akıcı bir Edebiyata döken Transpile Sihiri:

```coffeescript
# CoffeeScript dilinde Yorum Satirlari Kare/Hashtag (#) ile baslar (Ruby/Python gibi)!

# 1. DEĞİŞKEN (VAR) ATAMALARINI VE ARRAYLERİ TEMİZLEME
# Asla var, let, const yazmazsiniz! CoffeeScript onu cevirirken kendi ekler.
uygulamaIsmi = "Uzay Kesif Portali"

# Arrayleri arka arkaya diz, (virgul kullanmasan bile anlar - bazen)
gezegenler = ["Mars", "Jüpiter", "Satürn"]


# 2. FONKSIYON VE DÖNGÜ (OK İŞARETİ DEVRİMİ `->`)
# "function" kelimesi yasaktir! Onun yerine Ince OK (->) yazilir.
karsila = (kisi) -> 
    # Console logu calistir. Ayrica String Icine #{ } ile degisken enjekte et!
    console.log "Hos Geldin, Sevgili #{kisi}! Sistem Hazir."


# Array listesi ustunde Dongu at (Foreach mantigi) 
# Süslü parantez YOK, sadece içerden BOŞLUK (TAB) basarak kod blogunu belli et:
for gezegen in gezegenler
    console.log "Rota Olusturuluyor: " + gezegen


# 3. YALANCI NESNE YÖNELİMİ (CLASS MİMARİSİ EKLENTİSİ)
# JS'de (Fonksiyon Prototipleri) ile eziyet veren sinif yaratmayi Python zarafetine tasir!
class UzayGemisi
    
    # Otomatik YAPICI Metot (Constructor). Parametre onundeki (@) At-isareti demek:
    # "Disardan gelen bu parametreyi(pilot), DIREKT SINIFIN İÇİNE (this.pilot) OLARAK KAYDET!" 
    # (Bizi 'this.pilot = pilot' ameleliginden sonsuz kurtarir)
    constructor: (@pilot_adi, @motorGucu) ->
        @hiz = 0  # This yerine (@) kullanilir!
    
    # Sinifin Ic Fonksiyonu (Ok ile yapilir)
    hizlan: (gazaBasmaOrani) ->
        @hiz += gazaBasmaOrani * @motorGucu
        console.log "#{@pilot_adi} isimli pilot gaza basti. Hizim: #{@hiz}"


# Objeyi yarat (New) ve Fonksiyonunu Cagir!
gemim = new UzayGemisi("Yüzbaşi Emre", 50)
gemim.hizlan(2) 

```

Bu kodu dosyaya kaydettiğiniz zaman, Geliştirici klasöründeki Compiler bunu yakalar ve bir saniyede (Sizin C tabanlı Browser'lar okuyabilsin diye) binlerce satırlık `var gemim = ... UzayGemisi.prototype.hizlan = function() { ... }` JS iğrençliğine dönüştürür.

## Kimler Kullanır?
* Evrendeki bütün **Ruby on Rails** Geliştiricileri (2010 - 2017 dönemi). Rails paketinin içine gömülü (Default) gelirdi, tüm Web Full-Stack developerlar JS yerine mecburi/gönüllü Coffee içerdi.
* **GÜNÜMÜZDE:** Tamamen ÖLMÜŞTÜR (Obsolete/Terk edilmiştir). Neden? Çünkü JavaScript 2015 yılında çok büyük ve sert bir Evrim (ES6 Güncellemesi) geçirerek **CoffeeScript'in bulduğu bütün iyi şeyleri (Sınıflar `class`, Let/Const, `=>` Ok Fonksiyonları) kendi Ana-Diline Kopyaladı!**. 
* Dolayısıyla İnsanların JS yerine onu Transpile edecek Coffee eklentisine ihtiyacı kalmadı (Eski Ruby projelerinde miras kod olarak yaşayan çok değerli Atadır).
