# HCL (HashiCorp Configuration Language) / Terraform

## Özet
HCL (HashiCorp Configuration Language); 2014 yılında HashiCorp firması (Terraform'un yaratıcıları) tarafından icat edilen, şirketlerin devasa Bulut Bilişim (AWS, Google Cloud, Azure) altyapılarını farenizle (Mouse) tıklayıp kurarak değil, **"Kod Yazarak Kurmanızı (IaC - Infrastructure as Code)"** sağlayan, JSON'un yapısal mantığını YAML'ın okunabilirliğiyle birleştiren modern Altyapı ve Sistem Mimarisi Dilidir.

## Nedir ve Ne İşe Yarar?
Eskiden Amazon (AWS) üzerinde bir şirket sunucusu kurmak için, sistem yöneticisi (SysAdmin) AWS paneline tarayıcıdan girerdi. "Yeni Makine Oluştur" düğmesine basar, RAM'ini 16GB seçer, Güvenlik Duvarı kurallarını tık tık fareyle ayarlardı. Lakin yanlışlıkla bir ayarı(Mouse'la) sildiğinde Tüm sistem çökerdi ve Sistem Mimarisi aslından silindiği için "Geri Dönüş (CTRL+Z)" şansı olmazdı!

Terraform (ve Dili HCL) dediki: **Altyapıyı Kodlayalım! (IaC)** 
Artık mühendis bilgisayarına `sunucu.tf` diye kod dosyası yazar: "Bana 2 Adet 16GB Ram'li Sunucu Ver. Bir de Önüne Yük Dengeleyici (Load Balancer) Koy". Sonra terminale `terraform apply` yazar. Birkaç saniye içinde Milyar dolarlık Amazon tesisinde Sizin fiziksel sunucularınız çalışıp Ayağa Kalkar! Eğer o kodu silerseniz, Amazon'dan silinirler.

**Ne İşe Yarar?**
* **Bulut Bağımsız (Agnostik) Altyapı:** Amazon, Google veya Azure için (Hepsine uyumlu olarak) Altyapınızı yazabilirsiniz.
* **Versiyon Kontrolü (Altyapı Git Geçmişi):** Sunucunuzun Ayarları kod (HCL) metni olduğu için, Cihazların geçmişteki hallerini GitHub'da saklayabilir(Git History), bir stajyer yanlışlıkla Veritabanını silerse "Merge Revert (Eski Kod versiyonuna dön)" diyerek Sunucuyu Saniyesinde Geri Klonlatabilirsiniz.

## Dilin Mantığı ve Kod Yapısı
HCL, JSON ve YAML'a çok benzer ama asıl amacı "Kaynak Beyanı (Declaration)"dır.
Kod bloklarla tasarlanır. C++ gibi For veya While fonskiyonlarından çok, **`resource` (Hedef Kaynak)** ve **`variable` (Değişken)** Tanımlamalarıdır.  

Siz Terraform'a "Şu An Nasılsın?" veya "Nasıl yapacaksın?" demezsiniz. Sadece "NE ISTEDIĞINIZI" HCL ile beyan edersiniz, O gidip AWS'nin apisini Çekip o şekli Alır(State Machine Mimarisi).

### Örnek Bir HCL Kodu (Terraform ile AWS Sunucusu Kiralamak)
Aşağıda Bir Finans şirketinin Amazon'a Emir Vererek ("Mouse ile tıklamadan") Kendi Veritabanı Bulutunu Satın Alma/Kiralama Kodu `main.tf`:

```hcl
/* BU BIR HCL (Terraform) ALTYAPI KODUDUR */

# BİZİM HİZMET SAĞLAYICIMIZ: Amazon Web Services (AWS) OLACAK! 
# Kimligimizi(Key) Oraya tanimliyoruz
provider "aws" {
  region     = "us-east-1"      # Makineleri Nerede Kuracak? Amerikanin Dogusunda!
  access_key = "BENIM_GIZLI_AWS_IDM"
  secret_key = "COOOOK_GIZLI_SIFREM"
}

# 1. KAYNAK (RESOURCE): Yeni Bir Fiziksel/Sanal Sunucu (EC2) Istiyoruz!
resource "aws_instance" "benim_dev_sunucum" {
    
  # Hangi İşletim Sistemi Kursun? (Ubuntu 20.04'un AWS deki Kodu)
  ami           = "ami-04505e74c0741db8d" 
  
  # Makinenin Gucu Ne Olsun? t2.micro (1 GB Ram, Ucretsiz Deneme Surumu Mimarisi)
  instance_type = "t2.micro"

  # Etiketleme (Amazon Panelinde Ismi Ne olarak Gozukecek?)
  tags = {
    Name        = "Sirket-Ana-Makinesi"
    Environment = "Gelistirme (Dev)"
  }
}

# 2. KAYNAK (RESOURCE): Guvenlik Duvarı Kuralları (Firewall)
resource "aws_security_group" "ssh_izni" {
  name        = "sadece_ssh_acik"
  description = "22 Numaralı Porta (Uzaktan erisime) Izin ver"

  ingress {
    from_port   = 22             # Giren Port(Kapi) : 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]  # Tum Dunyadan Sifremi bilen Girebilir (IP kisitlamasi yok)
  }
}
```

Bu `main.tf` dosyasını Kaydedip, CMD'ye `terraform apply` Yazıp "Yes" Dediğiniz saniye, Amazon şirketinin sunucularıyla (API) haberleşilir ve Saniyeler içinde Amerika'da Taptaze, IP numarası olan Bir Bilgisayarınız Başlatılmış Olur. O Cihazın Bedeli (Faturası) Ay sonunda Kredi Kartınıza yansır. İşiniz bittiğinde `terraform destroy` yazdığınız an, Amazon o Makinenin fişini (Sildiğini) Onaylar. Fare (Mouse) ve Eziyet Yoktur!

## Kimler Kullanır?
* Bütün Büyük Kurumsal Şirketlerin **Cloud (Bulut) Mimarları ve DevOps / DevSecOps** Mühendisleri. (Trendyol, Spotify, Yemeksepeti, Netflix altyapıları Kodu olmadan Yaşayamaz).
* AWS, Azure, Google Cloud Ekosistemleri; Binlerce sunucudan oluşan devasa Orkestraları yönetebilmek (Bir felakette -Depremde- Veri merkezindeki Milyonlarca ayarı Avrupa'daki merkeze Saniyeler içinde Terrafrom Koduyla kopyalayıp kurabilmek) adına HCL dilini Baş Tacı yapmışlardır.
