# TOML (Tom's Obvious, Minimal Language)

## Özet
TOML (Tom'un Apaçık ve Minimal Dili); 2013 Yılında **GitHub'ın Yaratıcısı Olan Tom Preston-Werner** tarafından icat edilen, YAML'ın içerisindeki girinti(Indentation) zorluğunu ve beklenmeyen hatalarını Törpülemek (Yok Etmek) amacıyla çıkarılıp, Eski tarz Windows `.INI` Dosyalarının (Köşeli Parantez ve Eşittir) modernize edilmiş, şapşal derecede belirgin (Obvious) Okunması muazzam en kusursuz **Modern Ayar (Configuration) Dosyası** formatıdır.

## Nedir ve Ne İşe Yarar?
DevOps mühendisleri YAML'ı çok seviyordu ama YAML'ın "Boşluk (Space)" Takıntısı Kabus olabiliyordu. Bir satırı yanlışlıkla 3 Boşluk değil 2 Boşluk (Tab) bırakırsanız, Bütün Kubernetes sunucusu çökebiliyordu! 

Tom Preston-Werner, bu girinti belasına İsyan etti. Dediki: "Neden çok eski (1990 larin) o efsane `.INI` formatlarına Geri dönmüyoruz? Ayarlarımızı `anahtar = değer` Şeklinde DÜMDÜZ satırlara yazalım, Kimin altındaysa Başına Köşeli parantezle `[kategori]` yazalım!". 
TOML Böyle doğdu. Sürpriz yapmaz, Okuyan Kişiye şaka yapmaz. Adı Üstünde Apaçık (Obvious) ve nettir.

**Ne İşe Yarar?**
* **Modern Sistemlerin Yeni Konfigürasyon Kralıdır:** Rust Programlama dilinin devasa Paket Yöneticisi olan **Cargo** kütüphanesi Ayarlarını (Bagımlılıklarını) %100 TOML `Cargo.toml` ile çizer. Aynı şekilde Python evreni (Eski setup.py çöplerini bırakıp) Proje ayarlarını Resmi olarak `pyproject.toml` standartına devretmiştir.

## Dilin Mantığı ve Kod Yapısı
JSON gibi Tırnaklar Yoktur, YAML Gibi Boşluk Cezası Yoktur. `.INI` formatından Tek Farkı (Stringleri Tırnaklamak ve İç ice Objeleri kolayca Noktayla Cizebilmektir). 

Kavramlar:
*  Atamalar Eşittir (`=`) ile yapılır. 
*  Kategori / Gruplamalar Köşeli parantez `[SunucuAyarlari]` ile başlıklandırılır.

### Örnek Bir TOML Kodu: Python Veya Rust Projesi Bağımlılık Ayarı!
Aşağıda Bir Sunucunun veya Yazılımın Bütün Ayarlarının, Programlamayla alakasi olmayan Bir sekreter tarafindan bile okundugunda Hatasız anlanması amaciyla tasarlanan Yalinligi Cizilmektedir:

```toml
# BU BIR TOML (Ayar/Config) DOSYASIDIR!

# Temel Yapi (Eşittir Kullanilir)
title = "TOML Ornek Konfigurasyonu"

# Gruplama(Tablo / Obje Kavrami): Beden Objesinin Icine Girdik:
[sahip]
isim = "Tom Preston-Werner"
dogum = 1979-05-27T07:32:00-08:00 # Direk Tarih Veri Tipi Destekler (JSON'da bu bir Iskencedir!)

# YUVARLAKLI(Nested) Gruplama : database objesinin icindeki cluster'in Özelikleri 
[database.server_portlari]
aktifler = [ 8000, 8001, 8002 ]  # Dizi (Array) JSON'la Aynidir
max_baglanti = 5000
aciklama = "Eslestirme Sunucu Portlari"

# Array Objeleri Cift Koseli Parantezle [[]] (Listeler Temsil Eder):
[[kullanicilar]]
isim = "Ali"
yetki = "Admin"

[[kullanicilar]]
isim = "Ayse"
yetki = "SuperUser"
```

Dikkat edilirse, YAML daki O HÜZÜNLÜ "Boşluk Silersen Her Şey Patlar" korkusu burada Yoktur! Tüm satırlar sola dayalı bile olsa Program kimin ne ait olduğunu Üstündeki `[GrupBaşlığı]` 'ndan Kusursuzca (Obvious) anlar.

## Kimler Kullanır?
* Evrendeki bütün **Rust (Cargo)** ve **Modern Python (Poetry / PyProject)** Açık Kaynak Geliştiricileri. 
* Yazılımına Ayar Dosyası (Config) yapacak Bir Mühendis YAML ve JSON arasında Kalıyorsa, "Hacı Boşluk Belasıyla adamları korkutmayalım Dümdüz Verelim" diyerek Geleceğin Ayar Kütüphanesini TOML seçer. Go dilinde geliştirilen (Hugo gibi) Statik Site Jeneratörlerinin de Baş tacıdır. Her Yıl popüleritesi Üstel Artmaktadır.
