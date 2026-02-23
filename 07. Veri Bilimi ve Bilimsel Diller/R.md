# R

## Özet
R; 1993 yılında iki istatistikçi akademisyen (Ross Ihaka ve Robert Gentleman) tarafından Yeni Zelanda'da "S" dilinin devamı olarak yaratılmış, dünyanın gelmiş geçmiş en güçlü ve en zengin Açık Kaynaklı (Open Source) **İstatistiksel Analiz ve Veri Görselleştirme** programlama dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda ticari istatistik yazılımları (SPSS, SAS) çok pahalı ve özelleştirilemez kapalı sistemlerdi. Diğer yandan genel dillerle (C, Java) bir tıp verisini veya borsa olasılığını kodlamak (matematik formüllerini sıfırdan yazmak) aylar süren bir işkenceydi. 

R dili; "Bir yazılımcının değil, doğrudan Veri Bilimcilerin, Biyologların, Sosyologların ve İstatistikçilerin anladığı dilden konuşsun" felsefesiyle inşa edildi.

**Ne İşe Yarar?**
* **İstatistik ve Olasılık (Data Science):** Binlerce parametreli karmaşık doğrusal regresyon, lojistik analiz ve kümeleme (Clustering) algoritmalarını bünyesinde barındırır. Yeni bir ilacın hastalara etkisini haritalamak için tıp dünyasının resmi aracıdır.
* **Görselleştirme Sanatı (Data Visualization):** R'ın en büyük gücü `ggplot2` kütüphanesidir. Verilen saçma sapan Milyon satırlık bir Excel verisini saniyeler içinde New York Times veya akademik dergi kalitesinde muhteşem renkte, anlaşılır infografikler ve interaktif grafiklere dönüştürür.
* **Açık Akademik Ekosistem (CRAN):** Dünyadaki herhangi bir üniversite, yepyeni bir "Kanser hücresi modelleme" matematik formülü bulduğunda bunu anında R paketine (CRAN) yükler. C veya Java'da kimse gidip bu formülü bedavaya kütüphaneye dönüştürmez. Fakat R'da evrendeki her istatistik formülü zaten eklidir.

## Dilin Mantığı ve Kod Yapısı
Nesne Yönelimi (OOP - Sınıflar falan) kavramları ikinci plandadır. Veri Yönelimi (Data-Driven) bir dildir.

En kritik veri yapıları "Vektörler" ve "Data Frame"lerdir (Veri Çerçevesi - Bildiğiniz Excel tablosunu programlamanın dibine gömen bir zekadır). Değişken atamak için `=` işareti kullanılabilir ama R'ın kültürel (ve akademik) atama sembolü ok işaretidir: `<-`.  Mühendisler buna başta çok kızarsa da istatistikçiler işlemi soldaki bir "Hedefe" matematiksel eşitleme olarak gördüğü için bu sembol meşhur olmuştur.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin boy ve kilolardan oluşan 5000 satırlık verinin karesini alıp ortalamasını bulacaksınız. C dilinde iki satır `for` döngüsü kurarsınız. R dilinde liste/vektör düzeyinde döngü KULLANILMAZ (Kullanılması acemi işidir ve yavaştır). R, Vektörize bir dildir; siz Listeyi komple verirsiniz, formül o listenin bütün elemanlarına kendisi tek hamlede saldırır (`apply` mantığı).

### Örnek Bir R Kodu: Linear Regresyon ve Veri Çerçevesi (Data Frame)
Bir hastanedeki 4 hastanın Veri Setini saniyede simüle eden ve bu Yaş/Tansiyon ilişkisini istatistiksel Lineer Regresyon modeliyle (`lm`) çözen tipik (Akademik tarz) R sözdizimi:

```R
# R dilinde de Yorum satırı Python gibi '#' ile başlar.
# Hastaların yaş ve tansiyon verilerini Vektörler (c komutu) ile oluşturuyoruz:
yaslar <- c(45, 52, 65, 30, 71, 58) 
tansiyon <- c(120, 135, 150, 110, 170, 142)

# R'in MUCİZESİ (Data Frame): 
# Vektörleri anında birbiriyle birleştirip sanal bir Excel / SQL Tablosu yaratırız.
hasta_verisi <- data.frame(
  Yas = yaslar, 
  KanC_Basinci = tansiyon
)

# Lineer Regresyon Modellemesi (Tek satırlık şaheser):
# '~' (Tilde) işareti R'ın özel "Formül" sembolüdür.
# Anlamı: "Kan Basıncını etkileyen şeyin 'Yas' olduğunu farz et ve aralarındaki korelasyonu bul. Veri kaynağı = hasta_verisi".
tansiyon_modeli <- lm(KanC_Basinci ~ Yas, data = hasta_verisi)

# Sadece modelin istatistiksel, p-Value (Hata payı) vb. paha biçilmez ödev özetini ekrana basar:
summary(tansiyon_modeli)

# YENİ HASTA TAHMİNİ: Hastaneye 50 yaşında yeni birisi geldi, 
# Ürettiğimiz modele Soruyoruz: "Bu kisinin tahmini tansiyonu ne cikar?"
yeni_hasta <- data.frame(Yas = c(50))
tahmin_edilen_tansiyon <- predict(tansiyon_modeli, yeni_hasta)

# R'ın console (ekran) basımı fonksiyonları:
cat("Modelin 50 yaşındaki kişi icin tansiyon Tahmini: ", round(tahmin_edilen_tansiyon, 2), "\n")
```

Sadece 4-5 satır kodla tıp veya finans dünyasının ana tahminsel aracı olan yapay zeka istatistiği hazır hale gelmiştir. Aynısını standart bir C#, C++ dilinde kodlarken sayfalarca Olasılık formülü gömmeniz gerekirdi.

## Kimler Kullanır?
* Dünya çapındaki Akademisyenler, Üniversite araştırmacıları, epidemiyologlar (Salgın hastalık haritacıları).
* İlaç şirketlerinin "Veri Bilimcileri (Data Scientist)".
* R'ın en büyük eksiği; Saf Makine hızının yavaş (Interpreted) olması ve Genel (General-Purpose) olmadığı için bir Oyun veya Web Sitesi Server'i yazmaya pek müsait olmamasıdır. Bu yüzden sadece saf Veri Bilimi kullananlar (Örn: Veri Gazeteciliği) R kullanır.
