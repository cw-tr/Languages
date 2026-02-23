# Jinja2

## Özet
Jinja2; 2008 yılında Armin Ronacher (Ünlü Python mikro-framework'ü Flask'ın da yaratıcısıdır) tarafından geliştirilen, Python ekosisteminin en meşhur **Web Şablonlama Motorudur (Templating Engine)**. Amacı; HTML gibi statik (Aptal) işaretleme dillerinin içerisine `{% for %}` veya `{{ isim }}` gibi süslü parantezlerle "Python zekası (Değişkenler, Döngüler, If-Else)" enjekte ederek, arka plandaki veritabanından gelen Kullanıcı verisini Ekrana (HTML'e) dinamik olarak basmaktır.

## Nedir ve Ne İşe Yarar?
1990'larda Web siteleri Statikti (Sadece HTML idi). Eğer sitenizde 100 farklı ürün varsa, elinizle 100 tane farklı .html dosyası yazmak zorundaydınız.
Sonrasında PHP ve Python geldi. Python programcısının HTML ile konuşması gerekiyordu. Python kodunun içinde string olarak `html = "<html>" + kullanici_adi + "</html>"` yazmak çirkin ötesi bir Spagetti kodu doğuruyordu.

Armin Ronacher dedi ki: **"HTML ve Python'ı ayıracağız! Tasarımcı (Frontend) kendi HTML dosyasını yazsın. O HTML'in içine sadece benim belirlediğim `{{ }}` Bıyıklı Parantezleri koysun. Sunucu (Python) bu HTML'i ekrana yollamadan hemen önce(Render aşamasında) o bıyıkları bulup İçine Veritabanındaki 'Ahmet' ismini Gömecek!"**

**Ne İşe Yarar?**
* **Dinamik Web Siteleri (Server-Side Rendering):** Flask veya Django (Django'nun kendi motoru vardır ama Jinja'ya çok benzer) kullanıyorsanız; Veritabanından gelen "Sepetteki 5 Ürün" listesini alıp HTML'e yollarsınız. Jinja `{% for urun in sepet %}` koduyla o 5 ürünü Ekrana 5 tane `<li>` (Liste elemanı) olarak Kusursuzca Otomatik Çizer!
* **DevOps Konfigürasyonları (Ansible):** Ansible gibi Otomasyon araçları konfigürasyon dosyalarını (Örn: Nginx ayarları) Serverlara Yollarken JINJA2 motorunu kullanırlar. `server_name {{ sunucu_ipsi }}` yazılır, Ansible o `{{}}` lere Dinamik ipsiyi yapıştırarak milyonlarca cihaza Dağıtır.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Çift-Kıvırcık Bıyıklı Parantez `{{ }}`** (Sadece Değişkeni Ekrana Bas) ve **Yüzdeli Parantez `{% %}`** (Arka Planda Python Mantığı Çalıştır: If/For Dögüsü) üzerine kuruludur.

HTML dosyasının Uzantısına bakmaksızın İçine Saklanır.

### Örnek Bir Jinja2 Kodu (HTML İçerisine Python Döngüsü Gömmek)
Python (Flask) Sunucusundan "Ali, Veli, Ayşe" isimli liste geldiği senaryoda, Bunu ekranda HTML Tablosuna (VEYA Listesine) Otomatik Çeviren Şablon:

```html
<!-- BU BIR HTML DOSYASIDIR AMA ICINDE JINJA2 (Python Şablonu) SIZMISTIR! -->

<html>
<head>
    <!-- 1. DEĞİŞKEN YAZDIRMA (Variable Print)  {{ }} -->
    <title> Hoş geldin {{ kullanici.isim|capitalize }}! </title> <!-- capitalize = Jinja Filtresidir, ilk harfi Büyütür -->
</head>
<body>

    <!-- 2. IF KONTROLÜ (Sayfaya Sadece VIP olanlar Girebilsin!) {% %} -->
    {% if kullanici.vip_mi == True %}
        <h1 style="color: gold;">Sarayımıza Hoş geldiniz Sayın {{ kullanici.isim }}!</h1>
    {% else %}
        <h1>Standart Kullanıcı Arayüzü</h1>
    {% endif %} <!-- Unutmayın HTML aptal olduğu için END_IF Diye jinja'yı kapatmalıyız! -->


    <h2>Alışveriş Sepetiniz:</h2>
    <ul>
        <!-- 3. FOR DÖNGÜSÜ (Sepetteki Tüm Verileri Listeye Vur!) {% for %} -->
        {% for urun in sepetteki_urunler %}
        
            <!-- Eğer Ürün Kalmazsa HTML sınıfı (Stili) Değişsin Zekası: -->
            <li class="{% if urun.stok == 0 %}tukenmis{% else %}stokta{% endif %}">
                
                {{ urun.ad }} - {{ urun.fiyat }} TL
                
            </li>
            
        <!-- Liste Boş MU Geldi? Eğer 'Sepet Ürünleri' içinde Hiç veri yoksa, Aşağıyı çalıştırır (Mükemmel Jinja özelliğidir) -->
        {% else %}
            <li>Sepetiniz Bomboş! Lütfen Bir Şeyler Alın!</li>
            
        {% endfor %}
    </ul>

</body>
</html>
```
Eğer kullanıcı "VIP" değilse, O altın renkli `<h1>` etiketi **Müşterinin Tarayıcısına (Chrome'a) ASLA GİTMEZ!** Çünkü kod Sunucuda (Server-Side) Render (Bükülme) aşamasında iken Jinja Tarafından Kesilip Atılmıştır (Güvenlik). HTML Tarayıcıya Sadece Dümdüz Bir Metin Olarak (Son Haliyle) ulaşır. Müşteri Arkada Çalışan Python kodunu (Yada For döngüsünü) Ekranda Göremez F12'ye bassa Bile!.

## Kimler Kullanır?
* Python Dünyasındaki (Flask, FastAPI, Django) Bütün **Backend (Arka Yüz) ve Full-Stack Geliştiricileri**.
* Otomatik Mail Atma Sistemleri Yazanlar: "Sayın {{musteri_adi}} Kargonuz Yollanmıştır!" (Mail taslakları hep Jinja veya Mustache'dir).
* **DevOps Mühendisleri**: Makro Yazar gibi, Sistem konfigürasyonlarını Jinja "Template (Şablon)" leriyle bükerek Binlerce Sunucuya Kusursuz Atarlar (Ansiklopedimizdeki Ansible Tarafına Bakınız). Python Varsa Jinja Yanındadır.
