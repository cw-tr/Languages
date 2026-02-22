# Go (Golang)

## Özet
Go, 2009'da Google tarafından icat edilen; C dilinin acımasız derleme performansıyla, Python dilinin zevkli kısalığını birleştiren ve günümüzün tüm bulut internet altyapısını (Cloud/Backend) ve mikroservisleri taşıyan **modern sunucuların taçsız kralıdır**.

## Nedir ve Ne İşe Yarar?
Google sunucularında milyonlarca kod yığını C++ ve Java ile yazılıyordu. Java derlenmek ve açılmak için çok fazla RAM ve saniye tüketiyor; C++ kodlarının derlenmesi (hızından değil build süresinden) devasa bir Google verisinde 1 saat alıyordu. Ken Thompson (C ve UNIX'in yaratıcılarından) yanına iki efsane mühendisi daha (Rob Pike, Robert Griesemer) aldı ve Google için kendi dillerini ürettiler: **Go**.

Dilde `Sınıf (Class)` kavramı yoktur. `Kalıtım (Inheritance)` hiç yoktur. "Hızlı derlemeliyiz" ve "İşimiz bittiğinde bize sanal makine sormasın (JVM gibi), elimize pat diye doğrudan işletim sisteminde çalışan bağımsız (standalone) bir 0 ve 1 .exe dosyası versin" diyerek inşa edilmiş muazzam bir devrim.

**Ne İşe Yarar?**
* **Gezegenin Bulut ve Sunucu Ağı (Cloud Backend):** İnternetin çalışmasını sağlayan arka plan devasa bulut araçları (Docker, Kubernetes, Terraform) baştan aşağıya sadece GO dilinde yazılmıştır.
* **Eşzamanlılık - Aynanda Milyon İşlem (Concurrency):** 1.000.000 abonenin aynı anda bağlandığı bir WebSocket sunucusu (Mesajlaşma uygulaması) yazdığınızda Java veya C++ bunu eski usul ağır iş dizileriyle (Thread) çözerken kitlenir. Go, "Goroutine" adında kuş tüyü hafifsiklet sanal işçiler (işlemciler) kullanır. Milyonlarcası uyanıp aynı anda çalışırken sisteminizin yüzüne bakıp çay içebilirsiniz.
* **Mikroservis Mimarisi (Microservices):** Büyük E-Ticaret ve yemek sipariş firmalarının "Arama", "Sepet", "Fatura" kısımlarının ayrı ayrı küçük sunucular olması eyleminin en pratik uygulama dili oldu. Hızından dolayı sunucuya çok geç yüklenmez.

## Dilin Mantığı ve Kod Yapısı
Çok hızlı okunur. Tipler vardır ama zorunlu görünmez. Java'da on satırda import edilen ve hata (Exception) yönetilen `try catch` kalıplarından farklı çalışır. Hatalar diğer dillerde olduğu gibi uygulamayı "çökerten (crash)" bir bomba değil; **sıradan bir değişkendir**. Her fonksiyon hata çıktısı döner, Go programcısı ise "hata var mı" (if err != nil) diyerek bunu bir el bombası değil çakı gibi kontrol eder.

Sınıf (Class) yerine Struct (Veri yapı) kullanılır.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin internetten aynı anda 100 tane resim indireceksiniz. Go fonksiyonunun başına sadece `go` kelimesi eklersiniz. Ve bitti! Arka planda anında 100 tane minik çekirdek asenkron olarak hiçbir RAM kilitlenmesi yaşamadan o işlemi kollar (Goroutine). Thread'ler kilitlenmesin diye de `Channel` adında borular icat etmişlerdir ve parçalar veriyi bu borudan atıp haberleşir.

### Örnek Bir Go Kodu: "Goroutine" Sihri (%100 Paralellik)
Eski dillerin 50 satırlık iplik (Thread) kabusu yerine, sadece başına "go" yazarak fonksiyonun aynı anda 5 kopyasının milisaniyede nasıl koşturulduğunu (Ana makineyi hiç kitlemeden) izleyelim.

```go
// Standart "Sınıfsız (Class-sız)" sade yapılı Go Ana Dosyası
package main

// Gerekli zaman (time) ve yazdırma formati (fmt) araçları
import (
	"fmt"
	"time"
)

// Çalışacak herhangi bir fonksiyon: Uzun süren bir simülasyon veya İnternetten Veri İndirme diyelim.
func sunucudanVeriIndir(sunucuAdi string) {
	// Döngüler için gereksiz parantezler yoktur
	for i := 1; i <= 3; i++ {
		fmt.Printf("[%s] Sunucusundan %d. Paket Cekiliyor...\n", sunucuAdi, i)
		time.Sleep(1 * time.Second) // 1 saniye bilerek gecikme (Lag simülasyonu)
	}
}

// PROGRAM BAŞLANGICI
func main() {
	fmt.Println("Gorevler Baslatildi. Ana Uygulama Durmuyor...")

	// İŞTE GO MUCİZESİ (Goroutines): Fonksiyonun başına sadece 'go' ekleyin. 
	// Sistemi ASLA saniye geciktirmez veya kitlemez. 
	// Arka planda 3 farklı mini "iş parçacığı" anında kopyalanıp farklı kollara ayrılır.
	go sunucudanVeriIndir("DONGU_A (Trendyol)") 
	go sunucudanVeriIndir("DONGU_B (Getir)")
	go sunucudanVeriIndir("DONGU_C (Amazon)")   

	// Eğer ana uygulama anında biterse diğer 3 arka kol yarım kalır (Go hızlıdır kapanır!).
	// Onların kendi kendilerine çalıştıklarını C blokları kitlemeden ekranda görmek için: 
	// Ana kanala bilerek "ana program 4 saniye uyu bekle bakalım" komutu veriyoruz.
	time.Sleep(4 * time.Second)
	
	fmt.Println("Islemler Goroutines Seviyesinde Tamamlandi.")
}
```

## Kimler Kullanır?
* Kubernetes, Docker, Prometheus gibi **DevOps ve Bulut(Cloud)** sunucu araçlarının mimarları ve çekirdek (Core) yazılımcıları.
* Trendyol, Yemeksepeti, Twitch, Uber, Netflix gibi sisteminin arkasında saniyede milyonlarca tıklamayı donmadan (Lag/Request Timeout almadan) yanıtlaması icap eden dünyanın en devasa Microservice Backend Mühendisleri.
* Komut satırında çalışan, bağımlılığı olmayan (sıfır import ile çalışıp her linux makinedeki bir dosyada exelenen - CLI) pratik sistem ağ (Network) güvenlik araçlarını yazanlar.
