# PromQL (Prometheus Query Language)

## Özet
PromQL; SoundCloud şirketinden çıkıp (Kubernetes dünyasının 2 numaralı açık kaynak abisi haline gelen) **Prometheus** İzleme ve Uyarı (Monitoring & Alerting) sisteminin Kendi Kalbinden Yarattığı Özel bir Sorgu/Filtreleme Dilidir. Evrenin en farklı sorgu dilidir çünkü Onun derdi "Kullanıcının ismini Bulmak" DEĞİLDİR; **"Benim Evrendeki 100 Tane Sunucumun, Son 5 Dakikadaki Ortalama RAM (Bellek) Fırlamalarını (Zaman Serisi - Time Series) Matematiksel ve Grafiksel olarak ÇİZMEK"** İçin dizayn edilmiş bir Mühendislik/DevOps Silahıdır.

## Nedir ve Ne İşe Yarar?
Eğer bir Trendyol/Amazon sunucusu yöneticisiyseniz Sizin elinizde binlerce Makine(Server) çalışıyor Demektir.
Siz bu makinelerin ne durumda Olduğunu (Kaçı Patladı? Hangisinin Yükü Çok fazla) Her saniye Gidip İçlerinden (CPU_KULLANIM_ORANI: %80, RAM: %40) Diye Raporlarsınız Bu raporlar Prometheus Zaman motoruna Atılır.

PromQL Dedi Ki: "Siz Bana SQL Yazıp 'Diskini Göster' derseniz Cihazın SADECE ŞUANKİ Anını Görürsünüz. Lakin Zaman Serisi Verilerde (Grafiklerde) Her Saniye Milyonlarca Veri Akıyordur! Ben Size Öyle Bir Matematiksel Dil (PromQL) Veriyorum Ki: **Bana Son 15 Dakika İçindeki En yüksek Tepki (Hata/Gecikme) Miktarının Yüzdelik (99th Percentile) Dalgalanmasını Çiz Direkt Grafiğe Vurayım!!** "

**Ne İşe Yarar?**
* DevOps Uyarıları (Alerting / Alarm): Bir Sysadmin PromQL İçinde Şu Kodu Uyarı Olarak Yaratır: `Eger_Ortalama_CPU_Son_5_Dakikada_80_Vurduysa -> Benim Telefonuma Gece 3'te SMS (Slack) AT!`
* Grafana Ekranları: DevOps Cihazlarında (Grafana Dashboardlarında) O İzlediğiniz Renkli ve Asil "Borsa/Dolar Çizen" Dalgalanmaların Çizim Arka Planına dökülen Çizgili Mükemmel Matematik Dilleridir...

## Dilin Mantığı ve Kod Yapısı
Tamamen Kendi Fonksiyonları (Rate, Irates, Sum, Histogram_Quantile) olan, Süslü Parantezlerle `{etiket="x"}` Filtreleyen Ve Köşeli Parantezlerle `[5m]` ZAMAN DİLİMİ Kilitleyen, Aşırı Aşırı Matematiksel (Vektör-Bazlı) bir yapıdır.
Her Sorgunun Çıktısı Tablo/Yazı Değil; Gözünüzde Canlanacak Bir GRAFİK (Vektör Çizgisi) dir!

### Örnek Bir PromQL Kodu: (Netflix Sysamin'inin Tüm Makinelerindeki ÇÖKME Hatasını Bulması)
Diyelim ki Site Yavaşladı! Sistem Yönetiminin Terminale Veya Grafana Ekranına "Sorun hangi Makinede Ve Ortalama 1 Saniyedeki Hata (500 Error) Miktarı Ne?" diyerek Attığı Çılgın Kod: 

```promql
/* BU BİR PROMQL ALARM / GRAFİK SERİSİ SORGUSUDUR */

/* 1. MANTIK: Gelen HTTP (Site) İsteklerinden, Sadece STATUS = 500 (SERVER ÇÖKTÜ) Raporlarını Çek! */
/* (http_requests_total : Veritabanındaki İsimdir, Süslü parantezler İçindekiler Sadece Filtreler!!) */
http_requests_total{status="500"}

/* 2. DAHA İLERİ DÜZEY (RATE - YÜZDELİK (Ortalama Çıkarma ZAMAN MANTIĞI!!) */
/* Son "5 Dakikadaki [5m]" O ÇÖKME Hatasını Al; Gidip Matematiksel Hıza (Saniyedeki Ortalama Artışa - RATE) Çevir! */
rate(http_requests_total{status="500"}[5m])


/* 3. İLAŞAN SANAT (TOPLAMA MÜKEMMEL ALARMI MEYDANA GETİRME!) */
/* Evrendeki Tüm Sunucularınızdaki (Job='api') Bulunan Hataların Hızını Topla (SUM) - Ve Eğer Bu Toplam Saniyede 10'dan Büyükse BANA TELEFON AÇ!!! */
sum(
  rate(http_requests_total{job="api", status="500"}[5m])
) > 10

```
Çıkan Sonuç Yazıyla `True/False` Değildir! Bu Kod Ekrana Bir zaman Çizelgesi (Time-Series Graph) Vurur; Eğer Sonuca > 10 Yazarsanız, Grafiğin 10 Sayısını Aştığını Gördüğü Saniye Tüm Ofiste Kırmızı Işıklar Yanar, Alarm Sistemi Tetiklenir! DevOpslar Çayı Fırlatır (Incident Response)!


## Kimler Kullanır?
* Cihazlarının ve Organizasyonun (Google Cloud / AWS/ Kubernetes Sistemlerinin) Kalbini Dinleyen **Tüm DevOps (Site Reliability Engineers - SRE'ler)**.
* Bu dili Bilmeyen Bir DevOps veya Sistem yöneticisi "Kör Bir Doktora" Benzer. Cihazının Ateşini, Kalp Atışını (Healthcheck) ve Nabzını PromQL Yazarak Görselleştirir. Öyle Ki Sadece Sunucu değil, Evdeki Akıllı Çamaşır Makinesini de Metrik Bilgisini Birleştirip Bu Dille Grafikleştirir. Muazzam Zekice Bir Yapıdır.
