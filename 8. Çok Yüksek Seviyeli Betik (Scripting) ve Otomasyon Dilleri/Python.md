# Python

## Özet
Python; 1991 yılında Guido van Rossum tarafından, okunabilirliği ve kod sadeliğini her şeyin (hızın bile) önünde tutarak tasarlanmış, günümüz dünyasının tartışmasız en popüler, en yaygın ve **Yapay Zeka (AI) devrinin fiili (de facto) standart dili** haline gelmiş devasa bir scripting (betik) ve genel amaçlı programlama dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda C ve C++ ile kod yazmak, süslü parantezlerin, noktalı virgüllerin ve bellek yönetimi hatalarının içinde kaybolmak demekti. Guido van Rossum, "Kod yazmak İngilizce bir metin okumak kadar akıcı olmalı" diyerek Python'u yarattı. 

Python kodları doğrudan bilgisayar makinesine (İşlemciye 0-1 olarak) derlenmez. Python yoran ve okuyan (Interpreter/Yorumlayıcı) bir C programı (CPython) vardır. Siz Python kodunu yazarsınız, CPython bunu okur ve anında çalıştırır. Bu yüzden C diline göre çok daha **yavaştır**, ancak bir mühendisin (veya bilim insanının) hayata geçirme hızı (Development Speed) inanılmaz yüksektir.

**Ne İşe Yarar?**
* **Yapay Zeka (AI) ve Veri Bilimi:** ChatGPT'nin beyni, Tesla'nın Otopilotu, Midjourney'in görsel üretimi... Tüm bu Makine Öğrenimi (Machine Learning) ve Derin Öğrenme ekosistemi %99 oranında Python kütüphaneleri (PyTorch, TensorFlow, Pandas) ile yazılır. (Formüller Python ile çok kolay yazılır, arka plandaki ağırlık ise C++ motorlarına yaptırılır).
* **Web Geliştirme (Backend):** Instagram, Spotify, Netflix ve Reddit gibi trilyonluk platformların arka plan sunucu (Backend) sirkülasyonları Python (Django ve FastAPI framework'leri) ile saniyede binlerce isteği yanıtlayacak şekilde örülmüştür.
* **Otomasyon ve Scripting:** Bir klasördeki 10.000 resmi al, boyutunu küçült, siyah beyaza çevir ve buluta yükle. Bu tarz can sıkıcı gündelik işleri (Otomasyon betikleri) 5 satır Python koduyla 1 dakikada çözebilirsiniz. Siber Güvenlikçilerin (Beyaz Şapkalı Hackerların) birinci saldırı/savunma silahıdır.

## Dilin Mantığı ve Kod Yapısı
En radikal ve ikonik özelliği **Girinti (Indentation) sistemidir**. C, Java veya PHP dillerindeki gibi kod bloğunu açıp kapatan `{ }` süslü parantezler Python'da KESİNLİKLE YOKTUR. Kodun hangi bloğa (Örn: `if` veya `for` içine) ait olduğunu, satırın başındaki **Boşluklar (Tab/Space)** belirler. Bu kural sayesinde spagetti (karmakarışık) kod yazmak teknik olarak yasaktır, her Python kodu bir şiir gibi hizalı olmak zorundadır.

Tipler **Dinamiktir (Dynamic Typing)**. Bir değişkene `X = 5` dersiniz, iki satır aşağıda `X = "Merhaba"` diyebilirsiniz. Sistem çökmez. Değişkenin cebini anında genişletip String'e çevirir (İşlemcide ağır bir yüktür ama programcı için muazzam rahatlıktır).

**Örnek İşleyiş (Sembolik Olarak):**
Java'da ekrana bir yazı basmak için önce Klas(Class) tanımlayıp, `public static void main` yazıp, sonra `System.out.println("Merhaba");` yazmak gerekir. Python'da ise dümdüz boş bir text dosyası açar ve sadece `print("Merhaba")` yazar çalıştırırsınız. Bu kadar.

### Örnek Bir Python Kodu: Sözdizimi Şıklığı ve Makine Öğrenimi
Klasik bir Yapay Zeka (AI) kütüphanesini içe aktarıp, girintilerin nasıl parantezlerin yerini aldığını gösteren tertemiz bir Python 3 zarafeti:

```python
# Yorum satırları '#' ile başlar. "import" ile dev kütüphaneler çağrılır.
import random

# Sınıf (Class) yazmak mecburi değildir. 
# Fonksiyonlar 'def' (Define) ile tanımlanır. Parametrenin Tipini (Int, str) yazmak bile opsiyoneldir!
def sahte_yapay_zeka_tahmini(musteri_yasi, maasi):
    
    # İŞTE PYTHON MUCİZESİ (GİRİNTİLER): Süslü parantez ({ }) YOKTUR! 
    # 'if' satırının sonuna iki nokta (:) konur ve ALT SATIR'a bir TAB (4 boşluk) içeri girilir.
    # O 4 boşluk hizasında olan her şey o IF'in içindedir.
    
    if musteri_yasi < 18:
        print("Uyari: Musteri resit degil!")
        return 0  # Kredi Puanı Sıfır
        
    elif maasi > 50000:
        # İki nokta sonrası tekrar 4 boşluk içeriden başlarız
        sans_faktoru = random.uniform(1.0, 2.5)
        # F-String metni (Metin içine kolayca degisken gommek)
        print(f"Zengin Musteri Algilandi. Çarpan: {sans_faktoru}") 
        return maasi * sans_faktoru
        
    else:
        # Standart hesap
        return maasi * 0.5 

# PYTHON'DA DEĞİŞKENLER: Tipi asla belirtilmmez, Python kendisi (arka planda C objesi) bulur.
hasta_listesi = ["Ahmet", "Mehmet", "Ayşe"]  # Bu bir Liste (Array)
veritabani_sözlügü = {"ad": "Mars", "hiz": 24000} # Bu bir Sözlük (Dictionary/JSON) mantığı

# Fonksiyonu doğrudan en soldan hizadan çagırıp çalıstırırız:
kredi_skoru = sahte_yapay_zeka_tahmini(25, 60000)

print(f"Yapay Zeka Karari - Verilen Kredi Skoru: {kredi_skoru}")
```

Bütün kodlar bu kadar insansı bir akış ve İngilizce metin görünümündedir.

## Kimler Kullanır?
* Evrendeki bütün Yapay Zeka Geliştiricileri (AI Developers), Veri Bilimi Uzmanları, Data Analyst'ler. (OpenAI, Google DeepMind mühendisleri dâhil).
* Backend Web Geliştiricileri (Instagram ve Spotify'ın sunucu devrenleri bu kodu taşır).
* Finans analistleri (Otomatik Borsa algoritmaları ve borsa botları yazanlar).
* Güvenlik uzmanları (Hack araçları, Network Tarayıcılar), Sistem Adminleri (Sunucu otomatizasyonu için Bash yerine).
* Yeni programlamaya başlayan 10 yaşındaki bir çocuk ve oyun/robot prototipleyen donanım mühendisleri (Raspberry Pi üzerinden).
