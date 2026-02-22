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
    <!-- 1. DEGISKEN YAZDIRMA (Variable Print)  {{ }} -->
    <title> Hosgeldin {{ kullanici.isim|capitalize }}! </title> <!-- capitalize = Jinja Filtresidir, ilk harfi BWyutrrir -->
</head>
<body>

    <!-- 2. IF KONTROLU (Sayfaya Sadece VIP olanlar Girebilsin!) {% %} -->
    {% if kullanici.vip_mi == True %}
        <h1 style="color: gold;">Sarayimiza Hosgeldiniz Sayin {{ kullanici.isim }}!</h1>
    {% else %}
        <h1>Standart Kullanici Arayuzu</h1>
    {% endif %} <!-- Unutmayin HTML aptal oldugu icin END_IF Dİe jinja'yi kapatmaliyiz! -->


    <h2>Alisveris Sepetiniz:</h2>
    <ul>
        <!-- 3. FOR DONGUSU (Sepetterki Tum Verileri Lİsteye Vur!) {% for %} -->
        {% for urun in sepetteki_urunler %}
        
            <!-- Eger Urunn Kalmassa HTML sinifi(Stili) Degissiin Zekasi: -->
            <li class="{% if urun.stok == 0 %}tukenmis{% else %}stokta{% endif %}">
                
                {{ urun.ad }} - {{ urun.fiyat }} TL
                
            </li>
            
        <!-- Listen Bos MU Geldi? Eger 'Sepet Urunlari' icinde Hiiic veri yOONk sa, Asasigi COalıştiri(Mukkemel Jiinja ozelleihir)-->
        {% else %}
            <li>Sepetiniz Bombos! Lutfen Bir Seylee rAlin!</li>
            
        {% endfor %}
    </ul>

</body>
</html>
```
Eğer kullanıcı "VIP" değilse, O altın renkli `<h1>` etiketi **Müşterinin Tarayıcısına (Chrome'a) ASLA GİTMEZ!** Çünkü kod Sunucuda (Server-Side) Render (Bükülme) aşamasında iken Jinja Tarafından Kesilip Atılmıştır (Güvenlik). Html Tarayıcıya Sadece Dümdüz Bir Metin Olarak(Son Halilye) ulaşır. Müşteri Arkada Çalışan Python kodunu(Yada For dongusunu) Ekranda Göremez F12 ye basssa Bile!.

## Kimler Kullanır?
* Python Dünyasındaki (Flask, FastAPI, Django) Bütün **Backend (Arka Yüz) ve Full-Stack Geliştiricileri**.
* Otomatik Mail Atma Sistemleri Yazanlar: "Sayın {{musteri_Ad}} Kargonuz Yoladiri!" (Mail taslakaklariı heep Jıinja Veta Mıustacheedirr).
* **DevOps Muhendisleri**: Macro Yazar gbi, Sistem konnfigurasyonlarini Jinja "Template (Sablon)" Lerilye bükerek Binlercer SunuCuhyya Kusursuz Atarla r(Ansiklpedimiizndeki ASnsible Tarafina Bakiniix). Python Varsa Jinja Yanindadair.
