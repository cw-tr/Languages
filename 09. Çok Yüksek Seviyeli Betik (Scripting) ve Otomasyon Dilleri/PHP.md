# PHP

## Özet
PHP (PHP: Hypertext Preprocessor); 1994'te Rasmus Lerdorf tarafından yaratılmış, sadece ama sadece **İnternet Sitelerinin Arka Planını (Backend/Sunucu)** yönetmek ve dinamik web sayfaları (HTML) basmak için ihtisaslaşmış, geçmişte biraz kötü şöhreti olsa da günümüzde internetin %75'ini tek başına taşıyan devasa bir betik (Scripting) dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda internet siteleri sadece düz (statik) HTML yazılarından ibaretti. Siteye veritabanından bağlanan (Örn: "Hoşgeldin Mehmet" diyen) bir dinamizm eklemek C dili (CGI) tabanlıydı ve tam bir işkenceydi. 

Rasmus Lerdorf, kendi özgeçmişini ve web sitesine girenleri saymak için "Kişisel Ana Sayfa (Personal Home Page - PHP)" adında basit bir C programcığı serisi yazdı. O kadar hızlı ve pratik şekilde doğrudan HTML kodunun içine `<?php ... ?>` etiketiyle sızıp Veritabanı okuyabiliyordu ki, dil kontrolden çıktı ve bütün dünyayı ele geçirdi. Tıpkı Python gibi derlenmez, sunucudaki (Apache/Nginx) PHP-FPM motoru tarafından kodu tepeden aşağı yorumlanarak (Interpret) anında HTML'e dönüştürülüp tarayıcıya fırlatılır.

**Ne İşe Yarar?**
* **İnternetin %75'i:** Dünyanın en devasa İçerik Yönetim Sistemi (CMS) olan **WordPress**, baştan aşağı PHP ile yazılmıştır. Bugün internetteki tüm sitelerin üçte ikisi (Haber siteleri, Bloglar, Freelancer E-Ticaret/WooCommerce platformları) bu motor üzerinde günde milyarlarca tık alır.
* **Gelişmiş Backend Ekosistemi:** Wikipedia'nın veya Slack sunucularının tüm backend gücü PHP ile atar. Mark Zuckerberg Facebook'u yarattığında bütün dünyayı ilk PHP diliyle taşımış (daha sonra performansı artırmak için PHP'yi C++'a çeviren "Hack" adlı kendi HHVM derleyicisini yapmıştı).
* Modern dev E-Ticaret (Magento) altyapılarının ve Laravel kodlama çatısının (Framework) resmi dilidir.

## Dilin Mantığı ve Kod Yapısı
En temel özelliği, **HTML'in tam içine gömülebilen bir Parazit (veya Simbiyotik) dil** olmasıdır. Dümdüz tasarlanmış bir HTML web sayfası dosyasının (.php uzantılı) herhangi bir kelimesinin ortasına `<?php ... ?>` açıp içine Veritabanı sorgusu çakıp anında o kutuyu dinamik hale getirebilirsiniz.

Dilin en belirgin estetik kuralı, bütün değişkenlerin başında tartışmasız bir şekilde Dolar (`$`) işareti olması mecburiyetidir. `x = 5;` diyemezsiniz, mecburen `$x = 5;` dersiniz. Tüm bloklar `{ }` ile kapanır ve komutlar bitişi (`;`) ister. 

Eskiden (PHP 4-5) kuralsız ve Spagetti (Karmaşık) kod yazmaya çok müsaitti, devasa güvenlik açıkları (SQL Injection vb.) kolay yapılıyordu. Ancak PHP 7 ve PHP 8 sürümleriyle birlikte dile muazzam bir "Nesne Yönelimli (OOP)" ve katı Tiplendirme (Strict Types) zırhı giydirilmiş, Java kadar kurumsal bir hale bürünmüştür.

**Örnek İşleyiş (Sembolik Olarak):**
Sitenizi ziyaret eden kişinin oturumunu (Session) kontrol etmek ve "Profiline Git" butonu çizmek: `<?php if($giris_yapti_mi) { echo "<button>Profilim</button>"; } ?>`. Bu komut sunucuda okunur, sitenin kaynağında PHP kelimesi gizlenir, sadece butonu çizili (HTML'leşmiş) halde ziyaretçiye gider.

### Örnek Bir PHP Kodu: İç İçe (Simbiyotik) HTML ve Veritabanı Dizisi
Geleneksel olarak, C veya Java'daki gibi zorunlu Sınıf/Ana metod kalıpları yoktur. Dümdüz web sitesine entegre edilmiş, modern, Sınıf tabanlı bir OOP harikası (Laravel esintili Data DTO mantığı):

```php
<!-- BURASI NORMAL İNTERNET SİTESİ (Müşterinin Tarayıcısında Görünecek Katman) -->
<html>
<head><title>E-Ticaret Paneli</title></head>
<body>

    <h1> Günün Fırsat Ürünleri </h1>

    <?php 
    // İŞTE BURADAN İTİBAREN SUNUCU (PHP MOTORU) DEVREYE GİRER.
    // Bu kodlar müşterinin browserinda görünmez, sunucuda(Backend) islenir.

    // 1. ADIM: Güçlü ve Modern PHP (Sınıf ve Tip Dayatması)
    class UrunSistemi {
        
        // C# gibi yapıcı ve görünmez özellikler:
        public string $urunAdi;
        public int $fiyat;
        public bool $stoktaVarMi;

        public function __construct(string $isim, int $ucret, bool $stokDurumu) {
            $this->urunAdi = $isim;
            $this->fiyat = $ucret;
            $this->stoktaVarMi = $stokDurumu;
        }

        // HTML ciktisi ureten Metot
        public function htmlKartBas() {
            // "echo" komutu aninda sayfaya HTML kodunu firlatir:
            echo "<div style='border: 1px solid black; padding: 10px; margin: 5px;'>";
            echo "<h2>" . $this->urunAdi . "</h2>"; // '.' (Nokta) işareti String/Metinleri yan yana bantlar
            echo "<p> Fiyat: " . $this->fiyat . " TL </p>";
            
            // Satir ici modern kontrol
            if ($this->stoktaVarMi) {
                echo "<button style='color:green;'>Sepete Ekle</button>";
            } else {
                echo "<button style='color:red;' disabled>Tükendi</button>";
            }
            echo "</div>";
        }
    }

    // 2. ADIM: Veritabanindan çekilmiş gibi "Dolar($) İşaretli" PHP Değişken Dizisi Yaratiyoruz:
    $vitrinUrunleri = [
        new UrunSistemi("Akillı Telefon", 45000, true),
        new UrunSistemi("Oyun Konsolu 5", 25000, false),
        new UrunSistemi("Ergonomik Klavye", 2000, true)
    ];

    // 3. ADIM: Donguye Sokup Sayfaya (HTML Olarak) Basilmasını Emret!
    foreach ($vitrinUrunleri as $urun) {
        $urun->htmlKartBas(); 
    }
    
    // PHP İŞLEMİ BİTTİ, TEKRAR SUNUCUDAN ÇIK, SİTE HTML'ine DÖN:
    ?>

    <footer> Telif Hakki: Siber Magaza 2026. </footer>
</body>
</html>
```

Kullanıcı siteye girdiğinde arkaplandaki bu `$urun` değişkenlerini veya DB sorgularını ASLA görmez. Motor bu satırları tamamen temizler ve ziyaretçiye sadece mükemmel 3 adet Kutu (HTML div) gönderir. 

## Kimler Kullanır?
* Dünyadaki on milyonlarca WordPress Tema ve Eklenti (Plugin) Geliştiricisi ve Serbest Çalışan (Freelance) Webmaster'lar.
* **Laravel ve Symfony** gibi dünyaca ünlü çok kurumsal, inanılmaz güvenli, modern ve hızlı Web Geliştirme (Backend) Frameworklerini kullanan dev kurum mimarları.
* Hosting firmalarında en ucuz ve en hızlı yoldan (Sanal Makine kurmadan) "Hemen yayınlansın, dosyayı FTP ile sunucuya atıp siteyi açayım" rahatlığına erişmek isteyen bütün hızlı girişimci Web Hacker'ları.
