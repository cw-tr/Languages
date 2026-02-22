# Blockly

## Özet
Blockly; 2012 yılında Neil Fraser liderliğinde **Google** tarafından icat edilen/geliştirilen, doğrudan bir programlama dilinden ziyade; İnternet Tarayıcısı üzerinden (Browser-based) "Sürükle-Bırak Kod Blokları (Node/Puzzle) Fabrikası" kurmanızı sağlayan, Scratch felsefesini Javascript ile zırhlandırıp çocuk eğitiminden tutun Android Akıllı Telefon Programlamaya kadar (Örn: App Inventor) **Evrensel Blok-Kod Çevirmenliğine (Transpiler)** dönüştüren modern devasa API kitaplığıdır.

## Nedir ve Ne İşe Yarar?
Scratch efsaneviydi ancak kapalı bir kutuydu. Scratch'in bloklarını alıp kendi e-ticaret sitenize veya kendi mobil oyun motorunuza gömemiyordunuz, sadece eğitimdi. 

Google, "Kodlamayı Blok (Puzzle) şeklinde yapma fikrini Açık Kaynak Kütüphane yapalım. Geliştiriciler bu kütüphaneyi kendi Web Sitelerine eklesin. Kullanıcılar (Müşteriler/Öğrenciler) o menüden puzzle blokları sürüklesin, en güzeli de **Sürüklenen o bloklar Arka-Planda saniyesinde Gerçek Python, JavaScript, PHP, Lua veya Dart koduna metin olarak çevrilsin (Compile edilsin)!**" felsefesiyle Blockly'yi yarattı. 

**Ne İşe Yarar?**
* **Her Türlü Blok Uygulamasının Kalbi (Code.org vb):** Dünyadaki milyonlarca çocuğun sertifika aldığı (Obama'nın bile kullandığı) *Code.org* eğitim sitesinin bütün blokları Google Blockly mimarisidir. MIT'nin "App Inventor (Uygulama İcadı - Mobil App yapma sitesi)" Google Blockly kullanır.
* **Akıllı Ev Cihazı / IOT Programlama:** Elektronik (Arduino vs) ya da Ev Otomasyonu (Akıllı Işıklarım şöyle olursa böyle yap) sistemlerinin menülerinde son kullanıcıya kod yazdırmak için Blockly gömülür ve C++ veya JS'e çevirtilerek donanıma yollanır.

## Dilin Mantığı ve Kod Yapısı
Tamamen (Blockly Kütüphanesinin JavaScript diliyle siteye entegrasyonu) "Blok Sürükleme" kurgusudur ancak arkasındaki en büyük Zekası: **Kod Üreticisidir (Code Generator)**. 

Blockly'de bir "İF(Eğer)" bloğu alıp, İçine kırmızı "5" rakkamı, yanına eşitse Mavi Bir "X" çekerseniz;
Blockly API'si saniyesinde size şunu fırlatır:
- Javascript Olarak: `if (x == 5) { ... }`
- Python Olarak: `if x == 5:`
- Dart Olarak: `if (x == 5) { ... }`
Çünkü Blockly o bloğun "Metinsel Harf Değerlerinin (Macro Strings)" karşılıklarını jeneratör modüllerinde (Ağaç Parser) tutar.

**Örnek İşleyiş (Sembolik Olarak):**
Siz Ekranda Fareyle Yaptınız: [Döngü 3 Kere] -> [Print "Selam"]
Blockly Arkaplanında: Onu 1 salisede Python `for i in range(3): print("Selam")` a Çevirir ve Server'ınıza işlenmek üzere Saf Kod metni Atar!

### Örnek Bir Blockly Mantığı: Blok'un İçi Ve Gerçek Dünyaya Yansıması (Kodu Oluşturan Kod)
Blockly geliştiricisinin Web sitesine "Sizin yerinize Javascript üreten Yeni bir Sürükleme Modülü" ekleme ayarı: (Custom Block Definition)

```javascript
/* Blockly, Web gelistiricileri (Sirketler) tarafindan JS ile entegre edilir. */
// Sadece cocuklar icin degil IOT kontrolu yayan devlerin de Kod Fabrikasidir!

// 1. KENDİ ÖZEL BLOK'UNU (PUZZLE PARÇASINI) TASARLAMAK:
// "Akilli Lamba Yak" isminde Bir Blok Tasarliyoruz (Renk Rengi Ayarlaniyor vs)

Blockly.Blocks['akilli_lamba_yak'] = {
  
  // Bloğun Gorünüm Ayarlari: JSON tarzi (Web tasarimcisine özel) :
  init: function() {
    this.appendDummyInput()
        .appendField("Akilli Lambayi")
        // Kullanıci menüden (Dropdown) renk secsin:
        .appendField(new Blockly.FieldDropdown([["Kirmizi","RED"], ["Mavi","BLUE"], ["Yesil","GREEN"]]), "RENK")
        .appendField("rengiyle Tumuyle Yak!");
        
    this.setPreviousStatement(true, null); // Uzerine Baska blok takilabilir
    this.setNextStatement(true, null);     // Altina baska bok takilabilir
    this.setColour(160); // Yesilimsi Mavi Koyu Bir Blok Rengi
  }
};


// 2. KOD JENERATÖRÜ (O BLOK SÜRÜLENDİĞİNDE ARKA PLANDA ÇIKACAK OLAN SAF KOD!)

// Eger Projenizi PYHTON olarak Export aldiriyorsaniz:
Blockly.Python['akilli_lamba_yak'] = function(block) {
  
  // Bloktan Kullanicinin Sectigi Rengi Çek!
  var secilenRenk = block.getFieldValue('RENK');
  
  // PYTHON Çıktısı olan Metni Üret: (Donanima gidecek Saf Metin(Kod)!)
  var python_code_ciktisi = 'api_lamba_kontrol_et_modulu.lamba_guc_ver(color="' + secilenRenk + '")\n';
  
  return python_code_ciktisi;
};

// VEYA EGER Projenizi JAVASCRIPT Olarak Export aldiriyorsaniz (Aynu Ayna 2 Dil ureten fabrika!)
Blockly.JavaScript['akilli_lamba_yak'] = function(block) {
  var secilenRenk = block.getFieldValue('RENK');
  
  var js_code_ciktisi = 'smartHome.setLightColor("' + secilenRenk + '");\n';
  return js_code_ciktisi;
};
```
Sistem tam olarak böyle çalışır! Çocuk (veya amatör Müşteri) sadece Web'de Puzzle Birleştirir, Alt Katmanda Blockly (Sizin O bloğa eklediğiniz kurallara göre) binlerce satırlık hatasız C++/JS veya Python kodu yazarak Asıl İşlemciye Fırlatır.

## Kimler Kullanır?
* Dünyadaki tüm **Kodlama Eğitim Vakıfları (Code.org)**, Mobil Uygulama Geliştirme Kurguları (MIT App Inventor).
* Oyuncularının Kendi Harita (veya Kendi Zekalarını / Düşman Botlarını) yaratabildikleri Web Oyunlarına gömülen Geliştirici Firmalar. 
* Kendi otomasyon şirketini (Örneğin bir Sulama motoru sistemi) çiftçiye verirken, Çiftçi koddan anlamaz diye ona "Blok Sürükletip" arka planda C dilini otomatik yaratan Gömülü Donanım Mühendisleri. (Yazılımın geleceğini halkın en temeline indirgeyen dev teknolojidir).
