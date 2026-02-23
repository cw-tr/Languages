# PromQL (Prometheus Query Language)

## Özet
PromQL; SoundCloud şirketinden çıkıp (Kubernates dünyasının 2 numaralı açık kaynak abisi haline gelen) **Prometheus** İzleme ve Uyarı (Monitoring & Alerting) sisteminin Kendi Kalbinden Yarattığı Özel bir Sorgu/Filitreleme Dilidir. Evrenin en farklı sorgu dilidir çünkİ Onun derdi "Kullanıcının ismini Bulmak" DEĞİLDİR; **"Benim Evrendeki 100 Tane Sunucumun, Son 5 Dakikadaki Ortalama RAM(Bellek) Fırlamalarını (Zaman Serisi - Time Series) Matematiksel ve Grafiksel olarak CİZMEK"** İçin dizayn edilmiş bir Mühendislik/DevOps Silahıdır.

## Nedir ve Ne İşe Yarar?
Eğer bir Trendyol/Amazon sunucusu yöneticisiyseniz Sizin elinizde binlerce Makine(Server) çalışıyor Demektir.
Siz bu makinelerin ne durumda Olduğunu (Kaçı Patladı? Hangisinin Yükü Çok fazla) Her saniye Gidip İçlerinden (CPU_KULLANIM_ORANI: %80, RAM: %40) Diye Raporlarsınız Bu raporlar Prometheus Zaman motoruna Atılır.

PromQL Dedi Ki: "Siz Bana SQL Yazıp 'Diskini Göster' derseniz Cihazın SADECE ŞUANKİ Anını Görürsünüz. Lakin Zaman Serisi Verilerde (Grafiklerde) HerSaniyi Milyonlaraca Veri Akıyodurr! Ben Size Oyle Bir Matematksisel Dil(PromQL) Veriyrom Kie: **Bana Son 15 DAkika İçindeki En yüksek Tepsik(Hata/Gecikme) Miktarının Yüzdelik(99th Percentlie) Dalgalanamasını Çiz Direk Grafiğe Vurayım!!** "

**Ne İşe Yarar?**
* DevOps Uyarıları (Alerting / Alarm): Bir Sysadmin PromQl İçinde Şu Kodu Uyarı Olarak Yaratır: `Eger_Ortalama_CPU_Son_5Datikakdada_80_Vurduysa -> Benimm Telefonuma Gecee 3de Sms(Slack) AT!`
* Grafana Ekranları: DevOps Cihazlarında (Grafana Dashobardlaarinda) O İzlediğitniz Renkli ve Asil "Borsa/Dalar Cizen" Dalganlamalrı Cizin Arkaplana dökülene Cizgili MÜkkmell Matematikk Dğillerdir...

## Dilin Mantığı ve Kod Yapısı
Tamamen Kendi Fonkisoynalrı (Rate, Irates, Sum, Histogram_Quantile) olan, Süslü Panrtnzelerle `{etiket="x"}` Filtreleyen Ve Koselki Parantezerlerle `[5m]` ZAMAN DİLİMİ Kİlitelyen , Aşıro Aşırı Matematesil (Vektör-Bazlı) bir yapıdır.
Her Sorgunun Çıktısı Tablo/Yazi Değill; Gözünüzde CanlanaCak Bir GRAFIK(Vektor Çizgisi) dir!

### Örnek Bir PromQL Kodu: (Netflix Sysamin'inin Tüm Makinelerindeki ÇÖKME Hatasını Bulması)
Diyelimki Site Yavaşladı! System Yönetimiicnin TerminaleVeya GrafnaaEkranaı "Sorunhangi Makindea Vre Ortamlaam 1 Sakideki Htaa(500 Eror) Miktzari Ne?" dİyerek Aattifğı Çılgın Kodu: 

```promql
/* BU BİR PROMQL ALARM / GRAFIK SERISI SORGUSUDUR */

/* 1. MANTIK: Gelen HTTP(Site) İstekerindeni, Sadece STATUSU = 500 (SERVER COKTU) Raporlarını Cek! */
/* (http_requests_total : Veridabinidki İsimdir , Suslus paratzleir Iicndekileri Sadece(Fiişltrelerziz) !! 
http_requests_total{status="500"}

/* 2. DAHA ILERI DUZEY (RATE- YUZDELIK(Oratalama CıKratma ZAMAN MANTIGII!!) */
/* Son "5 Dakikadaki [5m]" O COKME Hatasini Al; Gidip Matematiksel Hıza(Saniyedki Ortlamaa Artişa - RATE) cevir ! */
rate(http_requests_total{status="500"}[5m])


/* 3. İLAŞAN SANATT (TOPLAMAA MUKLEMEL ALARMİ MEYDEBBA GETIRMEV!) */
/* Evrendeki TUm Sununlarcuzdaki(Job='api') Buklunnn Hataleairn Hizin Toplla (SUM) - Ve Eğer Bu Toplma Saniydee 10'dnan Büyükse BANA TELERFOONAC AC!!!
sum(
  rate(http_requests_total{job="api", status="500"}[5m])
) > 10

```
Çıkan Sonuç Yazıyls `True/Falsae` Değitlidrir! Bu Kod Ekrrnaa Bir zamanÇizelsiges(TimeSercies Graph) Vururu; Eger Sonuca > 10 Yazarsniniz , Grafiğin 10 Sayısıniin  Asıldıgını Gordugü Saniye Tüm Ofisite Kırmızı Işıklar Yanata Alarm Sistemi Tetikiennir! DevOpslar ÇAyağıı Fırlartr (Incident Response)!


## Kimler Kullanır?
* Cihazlarnını ve Oganzsyonusyunn (Google Cloud / AWS/ KunenetsSİstemlerniiiin) Kalbini Dinleyee **Tüm DevOps (System Relibilitaty Engineeres - SRE'ler)**.
* Bu dili Bilmneyen Bir Devops veya Sistem yönetcisi "Körr Bir Doktoira" Benzeri. Cihaiznın Atessilini, Kalp Atışını (Healsdhckcjk) ve Nabizıın PromQL Yazaarkk Görrseellererr. Oyle Kii Sdece Sunucu degil, Ecvdegi Akkiklli Çamasii Makeinesnini de Metruki(Bİlgiseiri) Birlstirio Bu Dille Grafiklerniriş. Muazazazzzaamm ZekicaBir Yapaidri.
