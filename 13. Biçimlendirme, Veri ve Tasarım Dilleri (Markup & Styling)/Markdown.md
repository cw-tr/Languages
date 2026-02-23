# Markdown

## Özet
Markdown; 2004 yılında John Gruber (ve Aaron Swartz) tarafından icat edilen, yazarların ve programcıların internet üzerinde Yazı (Makale/Doküman) yazarken HTML'in o iğrenç (Açma/kapatma) etiket `<b> </b>` karmaşasından kurtarmak maksadıyla, düz yazıya Sadece **Pürüzsüz Noktalama İşaretleri (Kareler, Yıldızlar) Ekleyerek** mükemmel HTML'e çevrilip renderlanmasını sağlayan Hafif (Lightweight) bir Biçimlendirme (Markup) dilidir. 

## Nedir ve Ne İşe Yarar?
2000'lerin başında "Okuyucuya güzel görünen Kalın Yazılı, Başlıklı, Kod Bloklu" yazılar internete asılmak isteniyordu. HTML kullanırsanız, makale şöyle korkunç görünüyordu:
`<p>İşte kodum <code> function() </code> gibi <b>şahanedir!</b>.</p>`

John Gruber, "Biz e-postaları birbirimize metin gönderirken ne kadar doğalız, Dikkat çekmek istediğimiz kelimenin önüne/arkasına İKİ YILDIZ koyuyoruz **Dikkat** gibi... O zaman bu insani e-mail alışkanlığını Yazılım motoruna çevirelim!" dedi.
Yani siz dümdüz bir `.md` dosyası açar, başlığın önüne Sadece Diyez/Hashtag `#` koyarsınız. Sistemi bunu OKUDUĞUNDA Anında 1 saniyede Tarayıcıya `<h1>` HTML Çıktısını atar. Mükemmel bir köprü aracıdır.

**Ne İşe Yarar?**
* **Yazılım Belgeleri Merkezi (GitHub Readme Krallığı):** GitHub gibi kod depolarında (Repository), her projenin girişinde bizi karşılayan devasa ve şık Açıklama Sayfaları (README.md dosyaları) %100 Oranda Markdown ile yazılmak Zorundadır. Geliştiriciler Dökümantasyon/Doc yazıtlarını (Python kütüphanelerinin kitapları vb) sadece HTML yazmadan Markdown'la derler.
* **Modern Not Uygulamaları ve AI Asistanları (ChatGPT vs):** Obsidian, Notion gibi devasa verimlilik not tutma yazılımları, Discord, Slack ve Yapay Zeka (ChatGPT) sohbetlerinin ekranınıza fırlattığı  **O Siyah/Gri arka planlı KOD Pencereleri** ve Tablolar tamamen Markdown gücüyle saniyesinde çevrilir! 

## Dilin Mantığı ve Kod Yapısı
Çok Basit, Ezberlemesi sadece 5 dakika Sürer!
- **Başlık (Header):** 1 Diyez `# Başlık 1 (Devasa)`. İki diyez `## Başlık 2 (Orta)`, Altı Diyez `###### Minik`.
- **Kalın/İtalik (Bold):** Metni VURGULAMAK için iki yıldız veya Cizgi içine al: `**Çok Kalın**` Veya `*Hafif Yatık (Italic)*`
- **Liste (List):** Alt alta çikolata, peynir yazmak için HTML deki UL ler yerine cümlenin başına Düz Cizgi (`-`)veya Yıldız (`*`) at: `- Peynir`
- **Köprü (Hyperlink/Tıklama):** Köşeli paranteze METNİ, Normal Paranteze LİNKİ ver. Şov: `[Ekle Beni](google.com)`

**Örnek İşleyiş (Sembolik Olarak):**
Yazdığınız (Raw) Markdown Metni : `# Olay! Ben **CyberWorld** den selam ile...`
Derleyicinin (Github vb) Chrome'a Çıkardığı Render (HTML) Karşılığı: `<h1> Olay! Ben <strong>CyberWorld</strong> den selam ile... </h1>`

### Örnek Bir Markdown Kodu: Açık Kaynak Projesi (README.md) Örneği
İşte "CyberWorld" (Şu an yazdığımız ansiklopedinin de formatı) gibi muazzam Github dokümanlarının mimari/çıplak ham madde gösterisi:

```markdown

# 🚀 CyberWorld: Uçsuz Bucaksız Teknoloji Ansiklopedisi

Bu proje, bir yapay zeka ajanının kullanıcısıyla senkronize olarak inşa ettiği, dünyanın en büyük **programlama ve teknoloji dilleri** devasa mühürlemesidir.

## 🛠️ Klasör ve Sistem Yapısı

Sistemler klasör klasör seviyelere ayırılmıştır. 
Öne çıkan (Favori) seviyeler:
* Level 1: Makinenin Dibi (Assembler Katmanı)
* Level 8: Scripting ve Hackerların Betikleri
* Level 11: Ezoterik (Cehennem Zihniyetli) Diller

### Uyarı Metin BLoğu (Blockquote)
Birilerine nasihat vereceğiniz o muazzam alıntı kutusu Büyüktür İşareti (`>`) ile akar:
> "Sistemde çalışmayan kod değil, okunmayan kod kötüdür!" - Efsane Atasözü

## 💻 Nasıl Kullanılır? (Kod Bloğu/Syntax Highlighting)
Eger belgenize "İncelenecek bir Terminal C++ vb KODU / Siyah EKRAN Kutusu" çizeceksiniz En büyük Sihir 3 Tane Ters-Tırnaktır (Backtick `) :

```python
# Kullanım Baslangici Test 
def uzayi_kesfet():
    print("Level 12 Basariyla Insa Edilldi!")
    
uzayi_kesfet()
```   <- (Uc adet tirnak bitirir)

Eğer sadece kelimenin ortasındaysa ufacık `print` yazısı gibi tek ters-tırnağa Alınarak gölgelendirilir (Inline code).

## 🗂️ Veri Tablosu Çizmek 
HTML'deki o igrenc (tr, td) kodlarına inat, Boru çizgisiyle (Pipe | ) Excel şeması yap:

| Dil Adı        | Bulunuş | Tasarımcı        |
| -------------- | ------- | ---------------- |
| Markdown       | 2004    | John Gruber      |
| BrainFuck      | 1993    | Urban Müller     |
| HTML           | 1990    | Tim B. Lee       |

--- (Uc Tire(---) sayfayi bastan sona Kesen bir Ayirici Yatay Çizgi (HR) atar!)
```
İşte bu metni bir Markdown okuyucusuyla Açarsanız karşınızda Resimli, tıklanabilir linkli, gölgeli dev bir modern Dergi sayfası parlar.

## Kimler Kullanır?
* Evrendeki bütün **Tüm Profesyonel Geliştiriciler (Programcı / Software Engineers)** Projenin dökümantasyonunu GitHub Readme üzerinden açıklamak ve Kod İncelemeleri (Code Review) yapmak için Markdown kullanır.
* Not tutma takıntısı olan **Araştırmacılar/Öğrenciler**. (Obsidian kullanıcıları hayatlarını `.md` formatında tutarak şirketlere/veritabanlarına mahkumiyeti reddeder.)
* Dünyanın En Pratik ve Kalıcı (Geleceğe Miras) düz-metin bilgisayar-insan ortak etkileşim sanatıdır.
